app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    pg: "https://github.com/stuarth/roc-pg/releases/download/v0.3/KSo0D5IZR-Ms1FSsl7jiRex_41CwktAbVGkqIqUt8wE.tar.br",
}

import pf.Stdout
import pf.Path exposing [Path]
import pf.Env
import pf.File
import pg.Cmd
import pg.Pg.Cmd
import pg.Pg.BasicCliClient
import pg.Pg.Result
import CodeGen
import Parse
import Queries
import Helpers exposing [taskAll]

main : Task {} [Exit I32 Str]_
main =
    Task.mapErr (run) \err ->
        when err is
            DatabaseUrlNotFound -> Exit 1 "DATABASE_URL not found"
            other -> Exit 1 "Error: $(Inspect.toStr other)"

run : Task {} _
run =
    databaseUrl = Env.var "DATABASE_URL" |> Task.mapErr! \_ -> DatabaseUrlNotFound

    client = Pg.BasicCliClient.connect! (databaseUrl |> parseConnectionString |> Task.fromResult!)

    sqlFiles = sqlFilesInDir! (Path.fromStr "./sql")

    sqlFilesStr = sqlFiles |> List.map Path.display |> Str.joinWith ", "

    Stdout.line! "Generating queries for $(sqlFilesStr).."

    parsedFiles = taskAll! sqlFiles Parse.parseSqlFile

    queries = taskAll! parsedFiles \f -> Queries.buildQuery f client

    codeGen! queries "Sql.roc"

    Task.ok {}

sqlFilesInDir : Path -> Task (List Path) _
sqlFilesInDir = \path ->
    Path.listDir! path
        |> List.keepIf \p -> Str.endsWith (Path.display p) ".sql"
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
