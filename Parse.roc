module [parseSqlFile]

import pf.Path exposing [Path]
import pf.File
import Helpers exposing [taskAll]

parseQueryFileContent = \queryContent ->
    lines = Str.split queryContent "\n"

    { comments, index: idx } = List.walkWithIndexUntil lines { comments: [], index: 0 } \acc, line, index ->
        trimmedLine = Str.trim line

        if Str.startsWith trimmedLine "--" then
            Continue { comments: List.append acc.comments (Str.replaceFirst trimmedLine "--" "" |> Str.trim), index }
        else
            Break { comments: acc.comments, index }

    { comments, query: (List.dropFirst lines idx) |> Str.joinWith "\n" |> Str.trim }

parseSqlFile : Path -> Task { comments: List Str, query: Str, name: Str } _
parseSqlFile = \path ->
    content = File.readUtf8! (Path.display path)

    { comments, query } = parseQueryFileContent content

    name = fnName path |> Task.fromResult!

    Task.ok { comments, query, name }

fnName = \path ->
    path |> filename |> Result.map \f -> Str.replaceLast f ".sql" ""

filename = \path ->
    when Str.split (Path.display path) "/" is
        [.., f] -> Ok f
        _ -> Err MissingFilename
