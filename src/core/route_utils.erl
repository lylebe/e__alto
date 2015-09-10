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

-compile([{parse_transform, lager_transform},export_all]).

errors_to_string([], _, AccIn) ->
	AccIn;
errors_to_string([H|T],Verbose,AccIn) ->
	errors_to_string(T, Verbose, err_to_string(H,Verbose) ++ AccIn).

err_to_string({duplicate,{R1,PID1},{_,PID2}},Verbose) ->
	case Verbose of
		true -> "Duplicate Entry Found " ++ R1 ++ " in " ++ PID1 ++ " AND " ++ PID2 ++ "~n";
		false -> R1 ++ " in " ++ PID1 ++ " AND " ++ PID2 ++ "~n"
	end;
err_to_string({Overlap1,Overlap2}, Verbose) ->
	case Verbose of
		true -> "Overlapping Entries Found " ++ Overlap1 ++ " overlaps " ++ Overlap2 ++ "~n";
		false -> Overlap1 ++ " overlaps " ++ Overlap2 ++ "~n"
	end.	

cidr_to_int(Cidr) when is_binary(Cidr) ->
	cidr_to_int(binary_to_list(Cidr));
cidr_to_int(Cidr) ->
	[Address|Mask] = string:tokens(Cidr,"/"),
	case inet:parse_address(Address) of
		{ok, FinalAddress} ->
			case length(Mask) of
				0 -> { FinalAddress, 32 };
				_ -> { FinalAddress, list_to_integer(lists:nth(1,Mask)) }
			end;
		_ ->
			lager:info("Result is error for parsing CIDR of ~p",[Cidr]),
			error
	end.
	
ip_to_int({A,B,C,D}) -> 
	(A*16777216)+(B*65536)+(C*256)+(D);	
ip_to_int({_,_,_,_,_,_,_,_}=IPv6Address) -> 
	ipv6_to_int(tuple_to_list(IPv6Address),0).
	
ipv6_to_int([],AccIn) ->
	AccIn;
ipv6_to_int([H|T],AccIn) ->	
	ipv6_to_int(T, (AccIn bsl 16)+H). 
	
base_address(CIDR) -> 
	lager:info("Called with value ~p",[CIDR]),
	case cidr_to_int(CIDR) of
		{Addr1, nil} -> ip_to_int(Addr1);
		{Addr2, Mask} -> base_address(Addr2,Mask)
	end.
	
base_address({_,_,_,_}=IPAddress, Mask) when is_integer(Mask) andalso Mask =< 32 ->
	(ip_to_int(IPAddress) bsr (32-Mask)) bsl (32-Mask);
base_address({_,_,_,_,_,_,_,_}=IPAddress, Mask) when is_integer(Mask) andalso Mask =< 128 ->
	(ip_to_int(IPAddress) bsr (128-Mask)) bsl (128-Mask). 

ip_range1(IPAddress, Mask, MaskLimit) ->
	%IPInt = ip_to_int(IPAddress),
	%Netmask = ip_to_int (to_netmask(Mask)),
	%LowVal = IPInt band Netmask,
	LowVal = base_address(IPAddress,Mask),
	HighVal = LowVal + (1 bsl (MaskLimit-Mask)) -1,
	[LowVal,HighVal]. 	

ip_range(IPAddress, Mask) when is_integer(Mask) andalso Mask =< 32 andalso size(IPAddress) =< 4 ->
	ip_range1(IPAddress, Mask, 32);
ip_range(IPAddress, Mask) when is_integer(Mask) andalso Mask =< 128 andalso size(IPAddress) =< 8 ->
	ip_range1(IPAddress, Mask, 128).

%to_netmask(Mask) when is_integer(Mask) andalso Mask =< 32 ->
	%Convoluted math...
%	PartialMask = 255 band ((16#ff bsr (8 - (Mask rem 8))) bsl (8 - (Mask rem 8))),
%	case (Mask div 8) of
%		0 -> {PartialMask, 0, 0, 0};
%		1 -> {255, PartialMask, 0, 0};
%		2 -> {255, 255, PartialMask, 0};
%		3 -> {255, 255, 255, PartialMask};
%		4 -> {255, 255, 255, 255}
%	end.
	
insert_route_interval([], IntervalTree, Errors) ->
	{ IntervalTree, Errors };
insert_route_interval([{Route,_}=Head|T], IntervalTree, Errors) ->
	{RouteBase,Mask} = cidr_to_int(Route),
	[L,H] = ip_range( RouteBase, Mask ),
	try
		NewTree = gb_trees:insert([L,H],{H,Head},IntervalTree),
		insert_route_interval(T, NewTree, Errors)
	catch
		error: { key_exists, _ } ->
				insert_route_interval(T, IntervalTree,
							[{duplicate,Head,element(2,gb_trees:get([L,H],IntervalTree))}] ++ Errors)
	end.
	
set_max_value({_, {[_,_], {_,_}, _, _}=Root}, CurrentTree) ->	
	% This is the initial call because the Size is present in the tuple
	{ MaxValue, FinalTree } = set_max_value(Root, CurrentTree),
	{ MaxValue, FinalTree };	
set_max_value({[Min,Max], {_,SomeValue}, nil, nil}, CurrentTree) ->
	{ Max, gb_trees:update([Min,Max],{Max,SomeValue},CurrentTree) };
set_max_value({[Min,Max], {_,SomeValue}, nil, Right}, CurrentTree) ->
	{ RightMax, NewTree } = set_max_value(Right, CurrentTree),
	FinalMax = erlang:max( RightMax, Max ),
	{ FinalMax, gb_trees:update([Min,Max],{FinalMax,SomeValue},NewTree) };
set_max_value({[Min,Max], {_,SomeValue}, Left, nil}, CurrentTree) ->
	{ LeftMax, NewTree } = set_max_value(Left, CurrentTree),
	FinalMax = erlang:max( LeftMax, Max ),
	{ FinalMax, gb_trees:update([Min,Max],{FinalMax,SomeValue},NewTree) };
set_max_value({[Min,Max], {_,SomeValue}, Left, Right}, CurrentTree) ->
	{ RightMax, NewTree } = set_max_value(Right, CurrentTree),
	{ LeftMax, NewTree2 } = set_max_value(Left, NewTree),
	FinalMax = erlang:max( erlang:max(Max, RightMax), LeftMax ),
	{ FinalMax, gb_trees:update([Min,Max],{FinalMax,SomeValue},NewTree2) }.

detect_overlaps({_,RootNode}=IntervalTree) ->
	L = overlaps( gb_trees:to_list(IntervalTree), RootNode, []),
	collect_overlaps(L,[]).
	
collect_overlaps([], AccIn) ->
	AccIn;
collect_overlaps([{contains,_,_,_}|T], AccIn) ->
	collect_overlaps(T, AccIn);
collect_overlaps([{overlaps,_,Item1,Item2}|T], AccIn) ->
	collect_overlaps(T, [{Item1,Item2}] ++ AccIn).
	
overlaps([], _, AccIn) ->
	AccIn;
overlaps([{Key,{_,SomeValue}}|T], RootNode, AccIn) ->
    OverlapsList = find_nondup_overlap(Key, RootNode, SomeValue, AccIn),
    overlaps(T, RootNode, OverlapsList).
	
nondup_overlap([X1,X2],[Y1,Y2]) ->
	case ((X1 /= Y1) and (X2 /= Y2) and (X1 =< Y2) and (Y1 =< X2)) of
		true -> 
			case (((X1 =< Y1) and (X2 =< Y2)) or ((Y1 =< X1) and (Y2 =< X2))) of
				false ->
					case (X1 =< Y1) and (Y2 =< X2) of
						true -> left_contains;
						false -> right_contains
					end;
				true -> overlaps
			end;
		false -> false
	end.

find_nondup_overlap(_, nil, _, AccIn) ->
	AccIn;
find_nondup_overlap([X1,_]=K1, {K2, {_,SomeValue}, Left, Right}, SomeData, AccIn) ->
	NextNode = case traverse_left(X1,Left) of
		false -> Right;
		true -> Left
	end,
	case nondup_overlap(K1, K2) of
		false -> find_nondup_overlap(K1, NextNode, SomeData, AccIn);
		overlaps -> find_nondup_overlap(K1, NextNode, SomeData, [{overlaps, K2, SomeValue, SomeData}] ++ AccIn);
		left_contains -> find_nondup_overlap(K1, NextNode, SomeData, [{contains, K2, SomeValue, SomeData}] ++ AccIn);
		right_contains -> find_nondup_overlap(K1, NextNode, SomeData, [{contains, K2, SomeData, SomeValue}] ++ AccIn)
	end.	
	
traverse_left(_, nil) ->
	false;
traverse_left(Low, {_, {LMax,_}, _, _}) ->
	LMax >= Low.

build_bitstring_trie([], Trie) ->
	Trie;
build_bitstring_trie([{CIDR,Data}|T], Trie) ->
	{BaseString,NetMask,BaseAddress} = case cidr_to_int(CIDR) of
		{Addr1, nil} ->
			{ip_to_int(Addr1),0,Addr1};
		{Addr2, Mask} -> 
			{base_address(Addr2,Mask),Mask,Addr2}
	end,
	[BinaryString] = io_lib:format("~.2B",[BaseString]),
	_Str = case (size(BaseAddress)) of
		4 -> %Pad to length.
				_Str1=lists:flatten(lists:duplicate(32-length(BinaryString),"0") ++ BinaryString);
		8 -> 
				_Str2=lists:flatten(lists:duplicate(128-length(BinaryString),"0") ++ BinaryString)
	end,
	Prefix = case (NetMask < 1) of
		true -> "0";
		false -> string:sub_string(_Str,1,NetMask)
	end,
	build_bitstring_trie(T, trie:store(Prefix,Data,Trie) ).
