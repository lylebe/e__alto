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
		 handle_endpoint_query/2,
		 terminate/3]).

-include("common_services.hrl").

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
	{"", cowboy_req:reply(422,Req), State}.

% An endpoint filter request
handle_endpoint_query(Req, State) ->
	handle_filter(fun endpointservices:ep_query/2,Req,State).
