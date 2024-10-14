module [buildQuery]

import pg.Cmd
import pg.Pg.Cmd
import pg.Pg.BasicCliClient
import pg.Pg.Result
import CodeGen

import Helpers exposing [taskAll]

buildQuery : { comments: List Str, query: Str, name: Str }, Pg.BasicCliClient.Client -> Task CodeGen.Query _
buildQuery = \{ comments, query, name }, client ->
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

    Task.ok { comments, query, name, inputs, outputs }

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
