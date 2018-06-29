-module(mab_strategy).
-export_type([bandit/0, mab_result/0]).

-type bandit() :: term().
-type mab_result() :: success | failure.

-callback init(Bandits :: list(bandit()), Args :: list()) -> {ok, State :: term()}.
-callback pull(State :: term()) -> {ok, State1 :: term(), Bandit :: bandit()}.
-callback result(State :: term(), Bandit :: bandit(), Outcome :: mab_result(), Extras :: list()) -> {ok, State1 :: term()}.
