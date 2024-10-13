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
import CodeGen

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

    client = Pg.BasicCliClient.connect! (databaseUrl |> parseConnectionString |> Task.fromResult!)

    queries = taskAll! sqlFiles \f -> parseSqlQuery f client

    codeGen! queries "Sql.roc"

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

parseSqlQuery : Path, Pg.BasicCliClient.Client -> Task CodeGen.Query _
parseSqlQuery = \path, client ->
    content = File.readUtf8! (Path.display path)

    { comments, query } = parseQueryFileContent content

    name = fnName path |> Task.fromResult!

    queryCmd = Pg.BasicCliClient.prepare! query { client, name }

    { kind } = Cmd.params queryCmd

    (parameters, fields) =
        when kind is
            PreparedCmd { parameters: ps, fields: fs } -> (ps, fs)
            _ -> ([], [])

    inputs = taskAll! parameters \parameter ->
        dataType = resolveDataTypeOid! client parameter.dataTypeOid

        Task.ok { parameter, dataType }

    outputs =
        fields
            |> taskAll! \output ->
                dataType = resolveDataTypeOid! client output.dataTypeOid

                Task.ok { output: { dataTypeOid: Num.toI32 output.dataTypeOid, name: output.name }, dataType }

    Task.ok { comments, query, path, fnName: name, inputs, outputs }

fnName = \path ->
    path |> filename |> Result.map \f -> Str.replaceLast f ".sql" ""

filename = \path ->
    when Str.split (Path.display path) "/" is
        [.., f] -> Ok f
        _ -> Err MissingFilename

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

codeGen = \queries, target ->
    when CodeGen.compile queries is
        Ok content ->
            Stdout.line! "Writing to $(target)"
            File.writeUtf8 target content

        Err e ->
            Stdout.line! "Error producing queries!"
            Task.fromResult (Err e)

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
