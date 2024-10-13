app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    pg: "https://github.com/stuarth/roc-pg/releases/download/v0.3/KSo0D5IZR-Ms1FSsl7jiRex_41CwktAbVGkqIqUt8wE.tar.br",
}

import pf.Stdout
import pf.Path exposing [Path]
import pf.File
import pf.Env
import pg.Cmd
import pg.Pg.Cmd
import pg.Pg.BasicCliClient
import pg.Pg.Result

main : Task {} [Exit I32 Str]_
main =
    Task.mapErr (run) \err ->
        when err is
            DatabaseUrlNotFound -> Exit 1 "DATABASE_URL not found"
            other -> Exit 1 "Error: $(Inspect.toStr other)"

run : Task {} _
run =
    databaseUrl = Env.var "DATABASE_URL" |> Task.mapErr! \_ -> DatabaseUrlNotFound

    sqlFiles = sqlFilesInDir! (Path.fromStr "./sql")

    sqlFilesStr = sqlFiles |> List.map Path.display |> Str.joinWith ", "

    Stdout.line! "Generating queries for $(sqlFilesStr).."

    queries = taskAll! sqlFiles parseSqlQuery

    client = Pg.BasicCliClient.connect! (databaseUrl |> parseConnectionString |> Task.fromResult!)

    codeGen! queries "Sql.roc" client

    Task.ok {}

sqlFilesInDir : Path -> Task (List Path) _
sqlFilesInDir = \path ->
    Path.listDir! path
        |> List.keepIf \p -> Str.endsWith (Path.display p) ".sql"
        |> Task.ok

parseQueryFileContent = \queryContent ->
    lines = Str.split queryContent "\n"

    { comments, index: idx } = List.walkWithIndexUntil lines { comments: [], index: 0 } \acc, line, index ->
        trimmedLine = Str.trim line

        if Str.startsWith trimmedLine "--" then
            Continue { comments: List.append acc.comments (Str.replaceFirst trimmedLine "--" "" |> Str.trim), index }
        else
            Break { comments: acc.comments, index }

    { comments, query: (List.dropFirst lines idx) |> Str.joinWith "\n" |> Str.trim }

parseSqlQuery = \path ->
    Task.map (File.readUtf8 (Path.display path)) \content ->
        { comments, query } = parseQueryFileContent content

        { comments, query, path }

fnName = \path ->
    path |> filename |> Result.map \f -> Str.replaceLast f ".sql" ""

filename = \path ->
    when Str.split (Path.display path) "/" is
        [.., f] -> Ok f
        _ -> Err MissingFilename

formatComments = \comments ->
    comments
    |> List.map \c -> Str.withPrefix c "# "
    |> Str.joinWith "\n"

resolveDataTypeOid = \client, dataTypeOid ->
    # https://github.com/giacomocavalieri/squirrel/blob/ab36e390c7428c4144e40cc75b9b31edbda85811/src/squirrel/internal/database/postgres.gleam#L46-L76
    typeQuery =
        """
        select
          -- The name of the type or, if the type is an array, the name of its
          -- elements' type.
          case
            when elem.typname is null then type.typname
            else elem.typname
         	end as type,
          -- Tells us how to interpret the firs column: if this is true then the first
          -- column is the type of the elements of the array type.
          -- Otherwise it means we've found a base type.
          case
            when elem.typname is null then false
            else true
         	end as is_array
        from
          pg_type as type
          left join pg_type as elem on type.typelem = elem.oid
        where
          type.oid = $1
        """

    Pg.Cmd.new typeQuery
    |> Pg.Cmd.bind [Pg.Cmd.u8 dataTypeOid]
    |> Pg.Cmd.expect1
        (
            Pg.Result.succeed
                (\type -> \isArray ->
                        { type, isArray }
                )
            |> Pg.Result.with (Pg.Result.str "type")
            |> Pg.Result.with (Pg.Result.bool "is_array")
        )
    |> Pg.BasicCliClient.command client

pgDataTypeToRoc = \type ->
    when type is
        "bool" -> Ok { type: Bool, decoder: "bool", bind: "bool" }
        "text" | "char" | "bpchar" | "varchar" -> Ok { type: Str, decoder: "str", bind: "str" }
        "float4" | "float8" | "numeric" -> Ok { type: F64, decoder: "f64", bind: "f64" }
        "int2" | "int4" | "int8" -> Ok { type: I64, decoder: "i64", bind: "i64" }
        _ -> Err (UnknownPgType type)

compileQuery : _, { comments : List Str, query : Str, path : Path } -> Task Str _
compileQuery = \client, { comments, query, path } ->
    queryFnName = fnName path |> Task.fromResult!

    queryCmd = Pg.BasicCliClient.prepare! query { client, name: queryFnName }

    { kind } = Cmd.params queryCmd

    (inputs, outputs) =
        when kind is
            PreparedCmd { parameters, fields } -> (parameters, fields)
            _ -> ([], [])

    paramaterDataTypes = taskAll! inputs \input -> resolveDataTypeOid client input.dataTypeOid
    parameters =
        paramaterDataTypes
        |> List.keepOks \dataType -> pgDataTypeToRoc dataType.type
        |> List.mapWithIndex \rocType, idx -> { rocType, name: "p$(Num.toStr idx)" }

    inputSignature = parameters |> List.map .name |> Str.joinWith ", " |> \signature -> if signature == "" then "{}" else signature

    binds =
        parameters
        |> List.map \{ rocType, name } -> "Pg.Cmd.$(rocType.bind) $(name)"
        |> Str.joinWith ", "

    baseIndentation = "        "

    indentedQuery = Str.replaceEach query "\n" "\n$(baseIndentation)"

    outputFields =
        outputs
            |> taskAll! \output ->
                Task.map (resolveDataTypeOid client output.dataTypeOid) \dataType -> { name: output.name, dataTypeOid: output.dataTypeOid, dataType }
            |> List.map \{ name, dataType } ->
                rocType = pgDataTypeToRoc dataType.type |> Result.withDefault { decoder: "X", type: Unknown, bind: "X" }

                { name, camelName: snakeCaseToCamelCase name, dataType, rocType }

    inputTypeSignature =
        parameters
        |> List.map \{ rocType } -> Inspect.toStr rocType.type
        |> Str.joinWith ", "
        |> \signature -> if signature == "" then "{}" else signature

    outputTypeSignature =
        outputFields
        |> List.map \{ camelName, rocType } -> "$(camelName): $(Inspect.toStr rocType.type)"
        |> Str.joinWith ", "
        |> \s -> "Cmd.Cmd (List { $(s) }) _"

    succeedSignature = outputFields |> List.map (\{ camelName } -> "\\$(camelName) ->") |> Str.joinWith " "
    resultWiths =
        outputFields
        |> List.map \{ name, rocType } -> "|> Pg.Result.with (Pg.Result.$(rocType.decoder) \"$(name)\")"
        |> Str.joinWith "\n$(baseIndentation)"

    succeedFn =
        """
        ($(succeedSignature)
            { $(outputFields |> List.map .camelName |> Str.joinWith ", ") }
        )
        """
        |> Str.replaceEach "\n" "\n$(baseIndentation)    "

    """
    $(formatComments comments)
    $(queryFnName) : $(inputTypeSignature) -> $(outputTypeSignature)
    $(queryFnName) = \\$(inputSignature) ->
        query =
            \"\"\"
            $(indentedQuery)
            \"\"\"

        Pg.Cmd.new query
        |> Pg.Cmd.bind [ $(binds) ]
        |> Pg.Cmd.expectN (
            Pg.Result.succeed
                $(succeedFn)
            $(resultWiths)
        )
    """
    |> Str.trim
    |> Task.ok

parseConnectionString : Str -> Result _ _
# :scream:
# note that this does not parse all possible connection string formats
parseConnectionString = \connectionStr ->
    # TODO fix error handling here -- empty string defaults make no sense
    { before: beforeSlash, after: afterSlash } =
        connectionStr
        |> Str.replaceFirst "postgres://" ""
        |> Str.splitLast "/"
        |> Result.withDefault { before: "", after: "" }

    { before: authPart, after: hostAndPort } =
        Str.splitFirst beforeSlash "@"
        |> Result.withDefault { before: "", after: beforeSlash }

    { user, auth } =
        Str.splitFirst authPart ":"
        |> Result.map \{ before, after } -> { user: before, auth: Password after }
        |> Result.withDefault { user: authPart, auth: None }

    { host, port } =
        Str.splitFirst hostAndPort ":" # should early return instead of defaulting to port 5432, but compiler bug prevents early return
        |> Result.map \{ before: h, after: p } -> { host: h, port: Str.toU16 p |> Result.withDefault 5432 }
        |> Result.withDefault { host: hostAndPort, port: 5432 }

    database = (Result.map (Str.splitLast afterSlash "?") \{ before: beforeQ } -> beforeQ) |> Result.withDefault afterSlash

    Ok {
        host,
        port,
        user,
        auth,
        database,
    }

compile : List { comments : List Str, query : Str, path : Path }, Pg.BasicCliClient.Client -> Task Str _
compile = \queries, client ->
    exportedFns = queries |> List.map .path |> List.keepOks fnName |> Str.joinWith ", "
    queryFns = queries |> taskAll! (\query -> compileQuery client query) |> Str.joinWith "\n\n"

    """
    # File generated by https://github.com/stuarth/rocky-the-flying-squirrel

    module [$(exportedFns)]

    import pg.Cmd
    import pg.Pg.Cmd
    import pg.Pg.Result

    $(queryFns)
    """
    |> Task.ok

codeGen = \queries, target, client ->
    Stdout.line! "Writing to $(target)"

    File.writeUtf8 target (compile! queries client)

findDirs : Str, Path -> Task (List Path) _
findDirs = \query, searchPath ->
    findDirsHelper [] (Path.listDir! searchPath) query

findDirsHelper : List Path, List Path, Str -> Task (List Path) _
findDirsHelper = \found, dirContents, query ->
    appendMatch = \acc, path ->
        when Path.display path |> Str.splitLast "/" is
            Ok { after } if after == query -> List.append acc path
            _ -> acc

    when dirContents is
        [path, .. as remainder] ->
            if Path.isDir! path then
                appendMatch found path
                    |> findDirsHelper! (Path.listDir! path) query
                    |> findDirsHelper remainder query
            else
                findDirsHelper found remainder query

        [] -> Task.ok found

# https://github.com/isaacvando/rtl/blob/77fa1b6872f9c4368f23ea124658953929f73729/rtl.roc#L133-L140
taskAll : List a, (a -> Task b err) -> Task (List b) err
taskAll = \items, task ->
    Task.loop { vals: [], rest: items } \{ vals, rest } ->
        when rest is
            [] -> Done vals |> Task.ok
            [item, .. as remaining] ->
                Task.map (task item) \val ->
                    Step { vals: List.append vals val, rest: remaining }

snakeCaseToCamelCase = \s ->
    words = Str.split s "_"

    upperLastWords =
        words
        |> List.dropFirst 1
        |> List.map \word ->
            letters = Str.toUtf8 word
            firstLetter = List.first letters |> Result.withDefault ' '

            upperFirst = if firstLetter >= 'a' && firstLetter <= 'z' then firstLetter - ('a' - 'A') else firstLetter
            Str.fromUtf8 (List.prepend (List.dropFirst letters 1) upperFirst) |> Result.withDefault word

    Str.joinWith (List.prependIfOk upperLastWords (List.first words)) ""

expect
    "helloWorld" == snakeCaseToCamelCase "hello_world"
