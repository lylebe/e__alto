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
	{Path,_}=mapservices:load_default_map(),
	%% Add the Default Map and the IRD mapped to "/" as the initial routes.
	e_alto_backend:set_constant(<<"routelist">>,[{map,Path},{ird,"/"}]).

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
    {_, DefaultRoute} = e_alto_backend:get_constant(<<"routelist">>),
    Dispatch = compileRouteList(DefaultRoute),
    {ok, _} = cowboy:start_http(alto_handler, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    lager:info("Cowboy started on port 8080",[]),
    e_alto_sup:start_link().

stop(_State) ->
    ok.

load_defaults() ->
	mapservices:load_default_map().

add_route(AppType,Route) ->
	process_route(AppType,Route,fun(AppTypeA,RouteA,List) ->  [{AppTypeA,RouteA}] ++ List end).

remove_route(AppType,Route) ->
	process_route(AppType,Route,fun(_,RouteA,List) -> lists:keytake(RouteA,2,List) end).

process_route(AppType,Route,E) when is_binary(Route) ->
	process_route(AppType,binary_to_list(Route),E);
process_route(AppType,Route,E) when is_list(Route) ->
	CurrentRouteList = e_alto_backend:get_constant(<<"routelist">>),
	NewList =  E(AppType,Route,CurrentRouteList),
	apply_route_changes(NewList).

reload_routes() ->
	{_, Routes} = registry:get_constant(<<"routelist">>),
	apply_route_changes(Routes).
	
apply_route_changes(List) ->	
	registry:set_constant(<<"routelist">>,List),
	CompiledRoutes = compileRouteList(List),
	cowboy:set_env(alto_handler, dispatch, CompiledRoutes).

compileRouteList(List) ->
	Routes = {'_', lists:foldl(fun({AppType,Path},AccIn) -> 
									GoodPath = case is_binary(Path) of
										false -> Path;
										true -> binary_to_list(Path)
									end,
									Handler = case AppType of 
										map -> map_services;
										ird -> ird_services;
										_ -> throw({error,unknown_apptype})
									end,
									[ {GoodPath, Handler, []} ] ++ AccIn
								end,
								[], List) },
	cowboy_router:compile([ Routes ]).