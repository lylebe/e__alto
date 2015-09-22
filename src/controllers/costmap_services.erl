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
%% @doc ALTO handler.
-module(costmap_services).

-define(TARGETMOD, costmapservices).

%% Standard callbacks for the behaviour.
-export([init/3,
		 allowed_methods/2,
		 content_types_provided/2,
		 content_types_accepted/2,
		 handle_costmap_get/2,
		 handle_costmap_filter/2,
		 terminate/3]).

-include("common_services.hrl").

allowed_methods(Req, State) ->
  	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"alto-costmap+json">>, []}, handle_costmap_get},
		{{<<"application">>, <<"alto-error+json">>, []}, handle_costmap_filter}
	], Req, State}.

content_types_accepted(Req, State) ->
  	{[
  		{{<<"application">>, <<"alto-costmapfilter+json">>, []}, handle_costmap_filter}
  	], Req, State}.

% A standard Map Service Query
handle_costmap_get(Req, State) ->
	{_Path,_}=cowboy_req:path(Req),
	io:format("~p--Path requested = ~p~n",[?MODULE,_Path]),
	{Body, Req2} = case registry:get_resourceid_for_path(_Path) of
		not_found ->
			lager:info("~p--Path Mapping not found for ~p~n",[?MODULE,_Path]),
			{ok, cowboy_req:reply(404,Req)};
		{_, _Resource} ->
			io:format("~p--Resource Id = ~p~n",[?MODULE,_Resource]),
			{mochijson2:encode( costmapservices:get_costmap(_Resource) ), Req}
	end,
	{Body, Req2, State}.

% A map filter request
handle_costmap_filter(Req, State) ->
	handle_filter(fun costmapservices:filter_costmap/2,Req,State).
