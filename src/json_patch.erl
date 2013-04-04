
-module(json_patch).
-include_lib("eunit/include/eunit.hrl").

-export([patch/2]).
-compile(export_all).       
-ifdef(EUNIT).
-compile(export_all).       
-endif.


path(Path) ->
    [<<>>| SPath] = binary:split(Path, <<"/">>, [global]),
    SPath.

patch(Patches, Data)  when is_binary(Patches) ->
    patch(jsx:decode(Patches), Data);
patch(Patches, Data) when is_binary(Data) ->
    jsx:encode(patch(Patches, jsx:decode(Data)));
        
patch([], Data) ->
    Data;
patch([JSON | Rest], Data) ->

    Path  = path(proplists:get_value(<<"path">>,	JSON, "/")),
    case proplists:get_value(<<"op">>, JSON) of
	<<"test">>	->
	    Value	= proplists:get_value(<<"value">>,	JSON),	    
	    {ok,Value}	= jsxd:get(Path, Data),
	    patch(Rest, Data);
	<<"remove">>	->
	    patch(Rest, jsxd:delete(Path, Data));
	<<"add">>	->
	    Value	= proplists:get_value(<<"value">>,	JSON),
	    patch(Rest, jsxd:set(Path, Value, Data));
	<<"replace">>	->
	    Value	= proplists:get_value(<<"value">>,	JSON),
	    patch(Rest, jsxd:set(Path, Value, Data));
	<<"move">>	->
	    From	= path(proplists:get_value(<<"from">>,	JSON)),	    
	    {ok, Value}	= jsxd:get(From, Data),
	    Data1	= jsxd:delete(From, Data),
	    Data2	= jsxd:set(Path, Value, Data1),
	    patch(Rest, Data2);
	<<"copy">>	->
	    From	= path(proplists:get_value(<<"from">>,	JSON)),	    
	    {ok, Value}	= jsxd:get(From, Data),
	    Data2	= jsxd:set(Path, Value, Data),
	    patch(Rest, Data2)
    end.
    

