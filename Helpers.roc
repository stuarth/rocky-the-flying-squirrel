module [taskAll]

# https://github.com/isaacvando/rtl/blob/77fa1b6872f9c4368f23ea124658953929f73729/rtl.roc#L133-L140
taskAll : List a, (a -> Task b err) -> Task (List b) err
taskAll = \items, task ->
    Task.loop { vals: [], rest: items } \{ vals, rest } ->
        when rest is
            [] -> Done vals |> Task.ok
            [item, .. as remaining] ->
                Task.map (task item) \val ->
                    Step { vals: List.append vals val, rest: remaining }
