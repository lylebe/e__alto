%% @doc ALTO handler.
-module(alto_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([return_directory/2]).

init(_Type, Req, _Opts) ->
    {ok, Req, no_state}.

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[
		{<<"application/alto-directory+json">>, return_directory}
	], Req, State}.

return_directory(Req, State) ->
	lager:info("Received IRD Request",[]),
	RawBody = registry:getIRD(),
	{ mochijson2:encode(RawBody), Req, State}. 
