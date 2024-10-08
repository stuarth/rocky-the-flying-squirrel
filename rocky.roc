app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
}

import pf.Stdout
import pf.Dir
import pf.Path exposing [Path]
import pf.File

main : Task {} _
main =
    sqlFiles = sqlFilesInDir! (Path.fromStr "./sql")

    sqlFilesStr = sqlFiles |> List.map Path.display |> Str.joinWith ", "

    Stdout.line! "found $(sqlFilesStr)"

    queries = taskAll! sqlFiles parseSqlQuery

    codeGen queries "Sql.roc"

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

queryInfo = \query ->
    { inputs: [], outputs: [] }

compileQuery : { comments : List Str, query : Str, path : Path } -> Str
compileQuery = \{ comments, query, path } ->
    # TODO fix
    name = fnName path |> Result.withDefault "X"
    { inputs, outputs } = queryInfo query

    fnInputs = List.prepend inputs "client" |> Str.joinWith ", "
    outputType = "{}"
    indentedQuery = Str.replaceEach query "\n" "\n        "

    """
    $(formatComments comments)
    $(name) : [] -> $(outputType)
    $(name) = \\$(fnInputs) ->
        query =
            \"\"\"
            $(indentedQuery)
            \"\"\"

        Pg.Cmd.new query
        |> Pg.Cmd.bind [ Pg.Cmd.u32 productId ]
        |> Pg.Cmd.expectN (
            Pg.Result.succeed {
                name: <- Pg.Result.str "name" |> Pg.Result.apply,
                price: <- Pg.Result.dec "price" |> Pg.Result.apply
            }
        )
        |> Pg.Client.command client
    """
    |> Str.trim

compile : List { comments : List Str, query : Str, path : Path } -> Str
compile = \queries ->
    exportedFns = queries |> List.map .path |> List.keepOks fnName |> Str.joinWith ", "
    queryFns = queries |> List.map compileQuery |> Str.joinWith "\n\n"

    """
    module [$(exportedFns)]

    $(queryFns)
    """

codeGen = \queries, target ->
    File.writeUtf8 target (compile queries)

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

taskAll : List a, (a -> Task b err) -> Task (List b) err
taskAll = \items, task ->
    Task.loop { vals: [], rest: items } \{ vals, rest } ->
        when rest is
            [] -> Done vals |> Task.ok
            [item, .. as remaining] ->
                Task.map (task item) \val ->
                    Step { vals: List.append vals val, rest: remaining }
