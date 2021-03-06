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
%% @doc Generic utilities used for costmaps and endpoint costmaps
%%
%% @end
-module(resources).

-export([
	next_id/0,
	next_id/1,
	resource_to_record/4,
	resource_to_record/7,
	resource_to_EJSON/1,
	resource_to_JSON/1,
	updateIRD/2
	]).
	
-include("e_alto.hrl").

-spec default_source() -> binary().
default_source() -> <<"local">>.

-spec next_id( Source :: binary() ) -> integer().
next_id(Source) when is_binary(Source) -> 
	_ReturnVal = case e_alto_backend:get_constant(Source) of
		not_found -> 0;
		{_,_Val} -> _Val
	end,
	e_alto_backend:set_constant(Source,_ReturnVal+1),
	_bval = integer_to_binary(_ReturnVal),
	<< Source/binary, <<"_">>/binary, _bval/binary >>. 

-spec next_id() -> integer().
next_id() ->  next_id( default_source() ).

-spec resource_to_record(Type :: atom(),
						 Name :: binary(),
						 URI :: binary(),
						 MediaType :: list()) -> #resourceentry{}.
resource_to_record(Type, Name, URI, MediaType)  when is_atom(type), is_binary(Name), is_binary(URI), is_list(MediaType) ->
	#resourceentry{type=Type, name=Name, uri=URI, mediatype=MediaType}.
	
-spec resource_to_record(Type :: atom(),
						 Name :: binary(),
						 URI :: binary(),
						 MediaType :: binary(),
						 Accepts :: binary(),
						 Capabilities :: list(),
						 Uses :: list()) -> #resourceentry{}.
resource_to_record(Type, Name, URI, MediaType, undefined, Capabilities, Uses) when is_atom(type), is_binary(Name), is_binary(URI), is_binary
(MediaType), is_list(Capabilities), is_list(Uses) ->
	#resourceentry{ type=Type, name=Name, uri=URI, mediatype=MediaType, accepts=undefined, capabilities=Capabilities, uses=Uses };
resource_to_record(Type, Name, URI, MediaType, Accepts, Capabilities, Uses) when is_atom(type), is_binary(Name), is_binary(URI), is_binary
(MediaType), is_binary(Accepts), is_list(Capabilities), is_list(Uses) ->
	#resourceentry{ type=Type, name=Name, uri=URI, mediatype=MediaType, accepts=Accepts, capabilities=Capabilities, uses=Uses }.

-spec resource_to_JSON(Resource :: #resourceentry{}) -> list().
resource_to_JSON(Resource) when is_record(Resource, resourceentry) ->
	mochijson2:encode(resource_to_EJSON(Resource)).

-spec resource_to_EJSON(Resource :: #resourceentry{}) -> tuple().
resource_to_EJSON(Resource) when is_record(Resource, resourceentry)  ->
	%%Process optional attributes first!
	_Accepts = gen_attr(<<"accepts">>, Resource#resourceentry.accepts, false),
	_Uses = gen_attr(<<"uses">>, Resource#resourceentry.uses, false),
	_Capabilities = gen_attr(<<"capabilities">>, Resource#resourceentry.capabilities,true ),
	_Attrs = [	{<<"uri">>, Resource#resourceentry.uri},
				{<<"media-type">>, Resource#resourceentry.mediatype }] ++ _Accepts ++ _Uses ++ _Capabilities,
	X={Resource#resourceentry.name, {struct, _Attrs}},
	lager:info("strucutre is ~p",[X]),
	X.

gen_attr(Name,Value,AsStruct) when is_binary(Value) ->
	case AsStruct of
		false -> [ to_resource_attr({Name,Value}) ]; 
		true -> [ {Name, {struct, to_resource_attr(Value) } }]
	end;
gen_attr(_,undefined,_) ->
	[];
gen_attr(Name,Value,AsStruct) ->
	case length(Value) of
		0 -> [];
		_ -> 
			case AsStruct of 
				false -> [ { Name, lists:foldl(fun(E,AccIn) -> [to_resource_attr(E)] ++ AccIn end, [], Value) } ];
				true ->  [ { Name, {struct, lists:foldl(fun(E,AccIn) -> [to_resource_attr(E)] ++ AccIn end, [], Value) } } ]
			end
	end.
	
to_resource_attr({Name, Value}) when is_binary(Name), is_list(Value) ->
	_Value = to_resource_attr(Value),
	case is_list(_Value) of
		false -> {Name, [_Value]};
		true -> {Name, _Value}
	end;
to_resource_attr(Item) when is_record(Item, costmetric) ->
	case  is_binary(Item#costmetric.name) of
		true -> Item#costmetric.name;
		false -> list_to_binary(Item#costmetric.name)
	end;
to_resource_attr(Item) when is_record(Item, resourceentry) ->
	case  is_binary(Item#resourceentry.name) of
		true -> Item#resourceentry.name;
		false -> list_to_binary(Item#resourceentry.name)
	end;
to_resource_attr(Item) when is_list(Item) ->
	lists:foldl(fun(E,AccIn) -> [to_resource_attr(E)] ++ AccIn end, [], Item);
to_resource_attr(Item) ->
	Item.

-spec updateIRD(Resource :: #resourceentry{},
				IRD :: tuple()) -> list().
updateIRD(Resource, IRD) when is_record(Resource, resourceentry) andalso is_tuple(IRD) ->
	_MetaInfo = resource_to_EJSON(Resource),
	ej:set({"resources",element(1,_MetaInfo)}, IRD, element(2,_MetaInfo)).
