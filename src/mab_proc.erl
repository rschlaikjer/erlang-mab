-module(mab_proc).
-behaviour(gen_server).

%% API

-export([
    pull/1,
    result/4,
    close/1
]).

%% Supervisor callback
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    strategy_mod :: module(),
    strategy_state :: term()
}).


start_link(Strategy, StrategyArgs, Bandits) ->
    gen_server:start_link(?MODULE, [Strategy, StrategyArgs, Bandits], []).

pull(Pid) ->
    gen_server:call(Pid, pull).

result(Pid, Bandit, Outcome, Extras) ->
    gen_server:cast(Pid, {result, Bandit, Outcome, Extras}).

close(Pid) ->
    gen_server:call(Pid, close).

init([Strategy, StrategyArgs, Bandits]) ->
    {ok, StratState} = Strategy:init(Bandits, StrategyArgs),
    State = #state{
        strategy_mod=Strategy,
        strategy_state=StratState
    },
    {ok, State}.

handle_call(pull, _From, State) ->
    {ok, StrategyState1, PullResult} = (State#state.strategy_mod):pull(
        State#state.strategy_state
    ),
    State1 = State#state{
        strategy_state=StrategyState1
    },
    {reply, {ok, PullResult}, State1};
handle_call(close, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({result, Bandit, Outcome, Extras}, State) ->
    {ok, StrategyState1} = (State#state.strategy_mod):result(
        State#state.strategy_state, Bandit, Outcome, Extras
    ),
    State1 = State#state{
        strategy_state=StrategyState1
    },
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
