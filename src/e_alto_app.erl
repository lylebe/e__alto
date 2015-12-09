% License: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of sthe License at
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
-module(e_alto_app).
-behavior(application).

-define(SUP_PID,supervisorpid).

%% Application callbacks
-export([start/0, start/2, start_app/2, stop/1, shutdown/0]).

-include ("core/e_alto.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================
initialize() ->
	AppList1 = [ goldrush, lager, crypto, ranch, cowlib, cowboy ],
	lists:foreach(fun application:start/1, AppList1),

	application:load(e_alto),
	e_alto_backend:init(),

lager:info("autoprops register start",[]),
	autogen_props:init(),
	lager:info("autoprops registered",[]),

	%% Add the Default Map and the IRD mapped to "/" as the initial routes.
	_DefaultRouteList = [{ird,"/"}],
	_ModulesToLoad = [ {map, fun mapservices:load_defaults/0}, 
					{costmap, fun costmapservices:load_defaults/0},
					{eps, fun endpointservices:load_defaults/0},
					{epcs, fun epcostservices:load_defaults/0} ],
					
	_Routes = lists:foldl(fun({ModuleType,LoadFunction},AccIn) ->
							_DefaultPaths = LoadFunction(),
							lists:foldl(fun(E,AccIn2) -> add_route_info(ModuleType, E, AccIn2) end, AccIn, _DefaultPaths)
						  end,
						  _DefaultRouteList,
						  _ModulesToLoad),

	e_alto_backend:set_constant(<<"routelist">>, _Routes),
	lager:info("Final Route List is ~p",[_Routes]).

add_route_info(_, {_,error}, List) ->
	List;
add_route_info(ApplicationType, {Path,X}, List) ->
	lager:info("Adding ~p, ~p and ~p",[Path,X,List]),
	[ {ApplicationType, Path} ] ++ List.

start() -> start_app(?APPLICATIONNAME).

start(_StartType, _StartArgs) ->
	initialize(),
    {_, DefaultRoute} = e_alto_backend:get_constant(<<"routelist">>),
    Dispatch = compileRouteList(DefaultRoute),
    {ok, _} = cowboy:start_http(alto_handler, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    lager:info("Cowboy started on port 8080",[]),
    lager:warning("start/2 exiting",[]),
    {ok, Pid} = e_alto_sup:start_link(),
    e_alto_backend:set_constant(?SUP_PID,Pid),
    {ok, Pid}.

shutdown() -> 
	stop(shutdown),
	exit(normal).

stop(_State) ->
	application:stop(cowboy),
    ok.

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
										costmap -> costmap_services;
										eps -> ep_service;
										epcs -> epcs_services;
										UnknownValue ->
											lager:info("An unknown type of ~p was referenced",[atom_to_list(UnknownValue)]),
											throw({error,unknown_apptype})
									end,
									[ {GoodPath, Handler, []} ] ++ AccIn
								end,
								[], List) },
	lists:foreach(fun({Path,Handler,_}) -> 
		lager:info("Configured Path = ~p with Handler = ~p",[Path,atom_to_list(Handler)])
	  end,
	  element(2,Routes)),
	cowboy_router:compile([ Routes ]).

%% Impressed by logplex's app starting so I borrowed some here.
start_app(App) -> start_app(App,transient).

start_app(App,Type) ->  start_app(App, Type, application:start(App, Type)).
	
start_app(_App, _Type, ok) -> ok;
start_app(_App, _Type, {error, {already_started, _App}}) -> ok;
start_app(App, Type, {error, {not_started, Dep}}) ->
    ok = start_app(Dep, Type),
    start_app(App, Type);
start_app(App, _Type, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).
