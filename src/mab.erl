-module(mab).
-compile(export_all).

tally(List) ->
    lists:foldl(
      fun(V, Acc) ->
        maps:update_with(
            V,
            fun(C) -> C + 1 end,
            1,
            Acc
        )
      end,
      #{},
      List
    ).

