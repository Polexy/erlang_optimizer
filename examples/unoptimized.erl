-module(unoptimized).

%% API
-export([func1/0, func2/0]).


%% MAPS
func1() ->
    Map = #{k => v},
    maps:get(k, Map).

func2() ->
    Map = #{k => v},
    maps:get(key, Map, undefined).

func3(_Any, _Any) ->
    %% maps:get(key, Map, undefined)
    maps:get(key, #{}, undefined), maps:get(key, #{k => v}, undefined),
    ok.