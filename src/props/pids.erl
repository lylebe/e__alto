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
%% @doc A module for handling PID Properties
%% @end

-module(pids).

-include("../core/e_alto.hrl").

-export([
	definitions/0,
	test_context/1,
	generate/1
	]).

pidname() -> <<"pid">>.

definitions() -> 
	[ #autogenprop{ identity = pidname(), testcontext_fun = fun pids:test_context/1, 
						gen_fun = fun pids:generate/1, displayname = binary_to_list(pidname()), prereqs=[] } ].

generate({Name,[Values]}) ->
	case lists:keyfind(pidname(), 1, Values) of
		{_,Val} -> Val;
		false -> mapservice:getPidForAddress(Name)
	end.

test_context({Name,[Values]}) ->
	case lists:keymember(pidname(), 1, Values) of
		true -> true;
		false -> utils:valid_ep(Name,nothing)
	end.
