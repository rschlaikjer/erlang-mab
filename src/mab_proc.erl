-module(mab_proc).
-behaviour(gen_server).

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

init([Strategy, StrategyArgs, Bandits]) ->
    {ok, StratState} = Strategy:init(Bandits, StrategyArgs),
    State = #state{
        strategy_mod=Strategy,
        strategy_state=StratState
    },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
