-module(mab).
-export([
    new/2,
    new/3,
    pull/1,
    result/3,
    result/4,
    close/1
]).

-spec new(module(), list(mab_strategy:bandit())) -> {ok, pid()} | {error, atom()}.
new(Strategy, Bandits) ->
    new(Strategy, Bandits, []).

-spec new(module(), list(mab_strategy:bandit()), list()) -> {ok, pid()} | {error, atom()}.
new(Strategy, Bandits, StrategyArgs) ->
    supervisor:start_child(mab_sup, [Strategy, StrategyArgs, Bandits]).

-spec pull(pid()) -> {ok, mab_strategy:bandit()} | {error, atom()}.
pull(Pid) ->
    mab_proc:pull(Pid).

-spec result(pid(), mab_strategy:bandit(), true | false) -> ok.
result(Pid, Bandit, Success) ->
    result(Pid, Bandit, Success, []).

-spec result(pid(), mab_strategy:bandit(), true | false, list()) -> ok.
result(Pid, Bandit, Success, Extras) ->
    mab_proc:result(Pid, Bandit, Success, Extras).

-spec close(pid()) -> ok.
close(Pid) ->
    mab_proc:close(Pid).
