# Rocky the Flying Squirrel

![image](https://github.com/user-attachments/assets/9ce5cbd8-3c51-4229-8d5b-9823085e91ae)

## What is it?

In short, Rocky takes a Postgres SQL query like

```sql
-- find an employee given their name
select * from employees where name = $1
```

and generates code to safely and ergonomically run the query using [`roc-pg`](https://github.com/agu-z/roc-pg)

```roc
# File generated by https://github.com/stuarth/rocky-the-flying-squirrel

module [selectEmployeeByName]

import pg.Cmd
import pg.Pg.Cmd
import pg.Pg.Result

# find an employee given their name
selectEmployeeByName : Str -> Cmd.Cmd (List { id: I64, name: Str, favoriteNumber: I64 }) _
selectEmployeeByName = \p0 ->
    query =
        """
        select * from employees where name = $1
        """

    Pg.Cmd.new query
    |> Pg.Cmd.bind [ Pg.Cmd.str p0 ]
    |> Pg.Cmd.expectN (
        Pg.Result.succeed
            (\id -> \name -> \favoriteNumber ->
                { id, name, favoriteNumber }
            )
        |> Pg.Result.with (Pg.Result.i64 "id")
        |> Pg.Result.with (Pg.Result.str "name")
        |> Pg.Result.with (Pg.Result.i64 "favorite_number")
    )
```

## Usage

### Installing Rocky

Build the `rocky` executable by cloning the repository and running 

```sh
roc build rocky.roc
```

And place `rocky` in your `$PATH`.

### Code Generation

Rocky looks for a folder called `sql` containing queries stored in separate `.sql` files, and creates a `Sql.roc` file containing functions corresponding to individual `.sql` files.

In order to understand queries' types, a database connection is needed. Set the `DATABASE_URL` environmental variable to the database's connection string when running `rocky`.

## Status

Rocky the Flying Squirrel is early! Notably abscent features coming soon

- [ ] Nullable columns
- [ ] Nullable columns due to joins
- [ ] Array types
- [ ] Bools
- [ ] DateTime

## Contribution

Contributions are welcome and appreciated!

## Inspiration

Rocky the Flying Squirrel is a port of the excellent [Squirrel](https://github.com/giacomocavalieri/squirrel) library written in Gleam by Giacomo Cavalieri.

## Notes

Rocky the Flying Squirrel depends on a fork of `roc-pg` until a [handful of changes it depends on](https://github.com/agu-z/roc-pg/pulls/stuarth) are merged.
