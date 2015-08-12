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
-module(ep_service).

-define(TARGETMOD, endpointservices).

%% Standard callbacks for the behaviour.
-export([init/3,
		 allowed_methods/2,
		 content_types_provided/2,
		 content_types_accepted/2,
		 handle_endpoint_query/2]).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  	{[<<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"alto-endpointprop+json">>, []}, err_resp}
	], Req, State}.

content_types_accepted(Req, State) ->
  	{[
  		{{<<"application">>, <<"alto-endpointpropparams+json">>, []}, handle_endpoint_query}
  	], Req, State}.

err_resp(Req, State) ->
	{"", cowboy_req:reply(422,Req), none}.

% An endpoint filter request
handle_endpoint_query(Req, State) ->
	io:format("~p--Endpoint POST Received~n", [?MODULE]),
	{_Path,_}=cowboy_req:path(Req),
	io:format("~p--Path requested = ~p~n",[?MODULE,_Path]),
	{ok, Body, _} = cowboy_req:body(Req),
	lager:info("Body received is ~p~n",[Body]),
	%Validation
	{RespBody, Req2} = case endpointservices:ep_query(_Path,Body) of
		{error, ErrMessage} ->
			{"", cowboy_req:reply(422,Req)};
		not_found ->
			{"", cowboy_req:reply(404,Req)};			
		{not_found, NFMessage} ->
			{"", cowboy_req:reply(422,Req)};		
		_FilteredMap ->
			io:format("Filter Map = ~p~n~n~n~n~n",[_FilteredMap]),
			{mochijson2:encode(_FilteredMap), Req}
	end,
	Req3 = cowboy_req:set_resp_body(RespBody,Req2),
  	{true, Req3, State}.
