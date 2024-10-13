module [compile, Query]

import pf.Path exposing [Path]

Input : {
    dataType : {
        isArray : Bool,
        type : Str,
    },
    parameter : { dataTypeOid : I32 },
}

Output : {
    dataType : {
        isArray : Bool,
        type : Str,
    },
    output : {
        dataTypeOid : I32,
        name : Str,
    },
}

Query : {
    comments : List Str,
    query : Str,
    fnName : Str,
    inputs : List Input,
    outputs : List Output,
    path : Path,
}

compile : List Query -> Result Str _
compile = \queries ->
    exportedFns = queries |> List.map .fnName |> Str.joinWith ", "
    queryFns = queries |> List.keepOks compileQuery |> Str.joinWith "\n\n"

    """
    # File generated by https://github.com/stuarth/rocky-the-flying-squirrel

    module [$(exportedFns)]

    import pg.Cmd
    import pg.Pg.Cmd
    import pg.Pg.Result

    $(queryFns)
    """
    |> Ok

compileQuery : Query -> Result Str _
compileQuery = \{ comments, query, fnName, inputs, outputs } ->

    parameters =
        inputs
        |> List.keepOks \{ dataType } -> pgDataTypeToRoc dataType.type
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
        |> List.map \{ output, dataType } ->
            rocType = pgDataTypeToRoc dataType.type |> Result.withDefault { decoder: "X", type: Unknown, bind: "X" }

            { name: output.name, camelName: snakeCaseToCamelCase output.name, dataType, rocType }

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
    $(fnName) : $(inputTypeSignature) -> $(outputTypeSignature)
    $(fnName) = \\$(inputSignature) ->
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
    |> Ok

formatComments = \comments ->
    comments
    |> List.map \c -> Str.withPrefix c "# "
    |> Str.joinWith "\n"

pgDataTypeToRoc = \type ->
    when type is
        "bool" -> Ok { type: Bool, decoder: "bool", bind: "bool" }
        "text" | "char" | "bpchar" | "varchar" -> Ok { type: Str, decoder: "str", bind: "str" }
        "float4" | "float8" | "numeric" -> Ok { type: F64, decoder: "f64", bind: "f64" }
        "int2" | "int4" | "int8" -> Ok { type: I64, decoder: "i64", bind: "i64" }
        _ -> Err (UnknownPgType type)

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
