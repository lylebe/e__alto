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
%% This is a generic header file for common macros and defaults.
%%

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
  ok.

set_error(Errors,Req) ->
	_Body = mochijson2:encode( utils:errors_toEJSON(Errors) ),
	{ok,Req2} = cowboy_req:reply(400,[{<<"Content-Type">>,<<"application/alto-error+json">>}],_Body,Req),
	Req2.

set_TextMessage(Code, Message,Req) ->
	{ok,Req2} = cowboy_req:reply(Code,[{<<"Content-Type">>,<<"application/text">>}],Message,Req),
	Req2.

handle_filter(FilterFunction,Req,State) ->
	io:format("~p--Filter POST Received~n", [?MODULE]),
	cowboy_debug:onrequest_hook(Req),
	{_Path,_}=cowboy_req:path(Req),
	io:format("~p--Path requested = ~p~n",[?MODULE,_Path]),
	{ok, Body, _} = cowboy_req:body(Req),
	lager:info("Body received is ~p~n",[Body]),
	%Validation & Processing
	case FilterFunction(_Path,Body) of
		{internal_error, IntErrorMessage} -> {halt, set_TextMessage(500,IntErrorMessage,Req), State};
		{error,Errors} -> 					 {halt, set_error(Errors,Req), State};
		not_found -> 						 {halt, cowboy_req:reply(404,Req), State};
		{not_found, NFMessage} ->			 {halt, set_TextMessage(404,NFMessage,Req), State};	
		GoodResult ->						 {true, cowboy_req:set_resp_body(mochijson2:encode(GoodResult), Req), State}	
	end.
