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
-module(map_services).

%% Standard callbacks for the behaviour.
-export([init/3,
		 allowed_methods/2,
		 content_types_provided/2,
		 content_types_accepted/2,
		 handle_map_get/2,
		 handle_map_filter/2]).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"alto-networkmap+json">>, []}, handle_map_get}
	], Req, State}.

content_types_accepted(Req, State) ->
  	{[
  		{{<<"application">>, <<"alto-networkmapfilter+json">>, []}, handle_map_filter}
  	], Req, State}.

% A standard Map Service Query
handle_map_get(Req, State) ->
	{_Path,_}=cowboy_req:path(Req),
	io:format("~p--Path requested = ~p~n",[?MODULE,_Path]),
	{Body, Req2} = case registry:get_resourceid_for_path(_Path) of
		not_found ->
			lager:info("~p--Path Mapping not found for ~p~n",[?MODULE,_Path]),
			{ok, cowboy_req:reply(404,Req)};
		{_, _Resource} ->
			io:format("~p--Resource Id = ~p~n",[?MODULE,_Resource]),
			{mochijson2:encode( mapservices:get_map(_Resource) ), Req}
	end,
	{Body, Req2, State}.

% A map filter request
handle_map_filter(Req, State) ->
	io:format("~p--Map Filter POST Received~n", [?MODULE]),
	{_Path,_}=cowboy_req:path(Req),
	io:format("~p--Path requested = ~p~n",[?MODULE,_Path]),
	{ok, Body, _} = cowboy_req:body(Req),
	lager:info("Body received is ~p~n",[Body]),
	%Validation
	{RespBody, Req2} = case mapservices:is_valid_filter(Body) of
		{false,_} -> 
			{"", cowboy_req:reply(422,Req)};
		{true, ParsedBody} ->
			case registry:get_resourceid_for_path(_Path) of
				not_found ->
					lager:info("~p--Path Mapping not found for ~p~n",[?MODULE,_Path]),
					{ok, cowboy_req:reply(404,Req)};
				{_, _ResourceId} ->
					io:format("~p--Resource Id = ~p~n",[?MODULE,_ResourceId]),
					_FilteredMap = mapservices:get_map_by_filter(_ResourceId,ParsedBody),
					io:format("Filter Map = ~p~n~n~n~n~n",[_FilteredMap]),
					{mochijson2:encode(_FilteredMap), Req}
			end
	end,
	Req3 = cowboy_req:set_resp_body(RespBody,Req2),
  	{true, Req3, State}.
