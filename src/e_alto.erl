% License: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
%% @author Lyle Bertz <lyleb551144@gmail.com>
%% @copyright Copyright 2015 Lyle Bertz
%%
%% @doc Extensions of the ej module (http://github.com/set/ej) to provide
%% support for JSON Merge Patch (RFC 7396) and JSON Patch (RFC 6902).
%% 
%% Base code for this is from 
%% 
%% @end
-module(e_alto).

%% Application callbacks
-export([start/0, start/2, stop/1, init/0, load_application/0]).

-include ("e_alto.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

init() ->
	application:start(compiler),
	application:start(syntax_tools),
	application:start(goldrush),
	application:start(lager),
	AppList = [
		crypto,
		ranch,
		cowlib,
		cowboy
	],
	lists:foreach(fun load_app/1, AppList),
	application:load(e_alto),
	e_alto_backend:init(),
	load_defaults().

load_app(App) ->
	 case application:start(App) of 
		ok-> lager:info("Applcation ~p started",[App]);
	    {error, Reason} -> lager:info("Application ~p start failed with Reason ~p",[App,Reason])
	  end.

start() ->
	init(),
	start([], []),
	ok.

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", alto_handler, []}]}
    ]),
    {ok, _} = cowboy:start_http(alto_handler, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    lager:info("Cowboy started on port 8080",[]),
    e_alto_sup:start_link().

stop(_State) ->
    ok.

load_defaults() ->
	lager:info("Loading Default Map",[]),
	_DefMapName = get_param(defaultmap),	
	_DefMapLoc = get_param(defaultmaploc),
	_DefMapPath = get_param(defaultmappath),
	%Parse the file
	lager:info("Begin File Read",[]),
	{ok, _File} = file:read_file(_DefMapLoc),
	lager:info("Read complete - Starting Storage"),	
	{ok, _ResourceId, X} = mapservices:set_map(_DefMapPath,_File),
	mapservices:set_default(_ResourceId),
	lager:info("Set map exited",[]),
	X.

