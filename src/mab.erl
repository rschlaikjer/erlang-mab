-module(mab).
-export([
    new/2,
    new/3,
    pull/1,
    result/3,
    result/4,
    close/1
]).

new(Strategy, Bandits) ->
    new(Strategy, Bandits, []).
new(Strategy, Bandits, StrategyArgs) ->
    supervisor:start_child(mab_sup, [Strategy, StrategyArgs, Bandits]).

pull(Pid) ->
    mab_proc:pull(Pid).

result(Pid, Bandit, Success) ->
    result(Pid, Bandit, Success, []).
result(Pid, Bandit, Success, Extras) ->
    mab_proc:result(Pid, Bandit, Success, Extras).

close(Pid) ->
    mab_proc:close(Pid).
