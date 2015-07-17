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
%% @end
-module(route_utils).

cidr_to_int(Cidr) ->
	[Address|Mask] = string:tokens(Cidr,"/"),
	case inet:parse_address(Cidr) of
		{ok, FinalAddress} -> 
			case length(Mask) of
				0 -> { FinalAddress, 32 };
				_ -> { FinalAddress, list_to_integer(Mask) }
			end;
		_ -> error
	end.
	
ip_to_int({A,B,C,D}) -> (A*16777216)+(B*65536)+(C*256)+(D).	

base_address(CIDR) -> 
	case cidr_to_int(CIDR) of
		{Addr1, nil} -> ip_to_int(Addr1);
		{Addr2, Mask} -> base_address(Addr2,Mask)
	end.
	
base_address(IPAddress, Mask) when is_integer(Mask) andalso Mask =< 32 ->
	ip_to_int(IPAddress) bor to_netmask(Mask).

ip_range(IPAddress, Mask) when is_integer(Mask) andalso Mask =< 32 ->
	IPInt = ip_to_int(IPAddress),
	Netmask = to_netmask(Mask),
	LowVal = IPInt bor Netmask,
	HighVal = LowVal + (1 bsl (32 - Mask)) -1,
	[LowVal,HighVal]. 
	
to_netmask(Mask) when is_integer(Mask) andalso Mask =< 32 ->
	%Convoluted math...
	PartialMask = 255 band ((16#ff bsr (8 - (Mask rem 8))) bsl (8 - (Mask rem 8))),
	case (Mask div 8) of
		0 -> {PartialMask, 0, 0, 0};
		1 -> {255, PartialMask, 0, 0};
		2 -> {255, 255, PartialMask, 0};
		3 -> {255, 255, 255, PartialMask};
		4 -> {255, 255, 255, 255}
	end.
	
insert_route_interval([], IntervalTree) ->
	IntervalTree;
insert_route_interval([{Route,_}=Head|T], IntervalTree) ->
	{RouteBase,Mask} = cidr_to_int(Route),
	[L,H] = ip_range( RouteBase, Mask ),
	insert_route_interval(T, gb_trees:insert([L,H],{H,Head},IntervalTree)).
		
set_max_value({[Min,Max], {_,SomeValue}, Left, Right}, CurrentTree) ->
	{ RightMax, NewTree } = set_max_value(Right, CurrentTree),
	{ LeftMax, NewTree2 } = set_max_value(Left, NewTree),
	FinalMax = erlang:max( erlang:max(Max, RightMax), LeftMax ),
	{ FinalMax, gb_trees:update([Min,Max],{FinalMax,SomeValue},NewTree2) };
set_max_value({[Min,Max], {_,SomeValue}, nil, Right}, CurrentTree) ->
	{ RightMax, NewTree } = set_max_value(Right, CurrentTree),
	FinalMax = erlang:max( RightMax, Max ),
	{ FinalMax, gb_trees:update([Min,Max],{FinalMax,SomeValue},NewTree) };
set_max_value({[Min,Max], {_,SomeValue}, Left, nil}, CurrentTree) ->
	{ LeftMax, NewTree } = set_max_value(Left, CurrentTree),
	FinalMax = erlang:max( LeftMax, Max ),
	{ FinalMax, gb_trees:update([Min,Max],{FinalMax,SomeValue},NewTree) };
set_max_value({[Min,Max], {_,SomeValue}, nil, nil}, CurrentTree) ->
	{ Max, gb_trees:update([Min,Max],{Max,SomeValue},CurrentTree) }.

detect_overlaps(IntervalTree) ->
	L = overlaps( gb_trees:to_list(IntervalTree), IntervalTree, []),
	collect_overlaps(L,[]).
	
collect_overlaps([{contains,_,_}|T], AccIn) ->
	collect_overlaps(T, AccIn);
collect_overlaps([{overlaps,_,Item1,Item2}|T], AccIn) ->
	collect_overlaps(T, [{Item1,Item2}] ++ AccIn).
	
overlaps([], IntervalTree, AccIn) ->
	AccIn;
overlaps([{Key,{_,SomeValue}}|T], IntervalTree, AccIn) ->
    OverlapsList = find_nondup_overlap(Key, IntervalTree, SomeValue, AccIn),
    overlaps(T, IntervalTree, OverlapsList).
	
nondup_overlap([X1,X2],[Y1,Y2]) ->
	case ((X1 /= Y1) and (X2 /= Y2) and (X1 =< Y2) and (Y1 =< X2)) of
		true -> 
			case (((X1 =< Y1) and (X2 =< Y2)) or ((Y1 =< X1) and (Y2 =< X2))) of
				true -> contains;
				false -> overlaps
			end;
		false -> false
	end.

find_nondup_overlap(_, nil, _, AccIn) ->
	AccIn;
find_nondup_overlap([X1,_]=K1, {K2, {_,SomeValue}, Left, Right}, SomeData, AccIn) ->
	NewData = case nondup_overlap(K1, K2) of
		false -> [];
		overlap -> {overlap, K2, SomeValue, SomeData};
		contains -> {contains, K2, SomeValue, SomeData}
	end,
	case traverse_left(X1, Left) of
		false -> find_nondup_overlap(K1, Right, SomeData, [NewData] ++ AccIn);
		true -> find_nondup_overlap(K1, Left, SomeData, [NewData] ++ AccIn)
	end.	
	
traverse_left(Low, nil) ->
	false;
traverse_left(Low, {_, {LMax,_}, _, _}) ->
	LMax >= Low.

build_bitstring_trie([], Trie) ->
	Trie;
build_bitstring_trie([{CIDR,Data}|T], Trie) ->
	{BaseString,NetMask} = case cidr_to_int(CIDR) of
		{Addr1, nil} -> {ip_to_int(Addr1),0};
		{Addr2, Mask} -> {base_address(Addr2,Mask),Mask}
	end,
	BinaryString = io_lib:format("~2B.",[BaseString]),
	%Remove the tail of the BinaryString
	Prefix = string:sub_string(BinaryString,1,32-NetMask),
	build_bitstring_trie(T,trie:store(Prefix,Data,Trie)).
	
	
