-module(e_alto_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

%% API.
-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.
init([]) ->
	Procs = [],
	{ok, {{one_for_one, 10, 10}, Procs}}.
