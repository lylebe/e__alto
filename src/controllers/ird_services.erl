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
%% @doc ALTO handler for the Information Resource Directory
-module(ird_services).

%% Standard callbacks for the behaviour.
-export([init/3,
		 allowed_methods/2,
		 content_types_provided/2,
		 content_types_accepted/2,
		 handle_ird_get/2]).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  	{[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>,<<"alto-directory+json">>, []}, handle_ird_get}
	], Req, State}.

% No Posts are accepted for the IRF
content_types_accepted(Req, State) ->
  	{[], Req, State}.

handle_ird_get(Req, State) ->
	io:format("IRD GET Request Received", []),
	Body = mochijson2:encode(registry:getIRD()),
	{Body, Req, State}.
