%% @private
-module(mab_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
	Procs = [{mab_proc, {mab_proc, start_link, []},
		transient, 5000, worker, [mab_proc]}],
	{ok, {{simple_one_for_one, 10, 10}, Procs}}.
