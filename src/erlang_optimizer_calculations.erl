-module(erlang_optimizer_calculations).
-include("../include/erlang_optimizer_headers.hrl").


%% API
-export([calculate/0]).


calculate() ->
    N = ?TEST_NUMBERS,
    Map = generate_map(?TEST_MAP_SIZE),
    #{
        map_match_vs_get_2 => func_diff(fun test_map_match/2, fun test_maps_get_2/2, [N, Map]),
        get_2_vs_get_3 => func_diff(fun test_maps_get_2/2, fun test_maps_get_3/2, [N, Map])
    }.

%% support functions
func_diff(Func1, Func2, Args) ->
    Res = lists:sum(
        [begin
             {T1, _} = timer:tc(Func1, Args),
             {T2, _} = timer:tc(Func2, Args),
             (T1 - T2)/T2
         end || _X <- lists:seq(1, ?TEST_CYCLES)]
    ) / ?TEST_CYCLES,
    {Res, io_lib:format("~.2f%", [Res * 100])}.


%% MAPS
generate_map(Size) when Size > 2 ->
    generate_map(Size, 0, #{}).

generate_map(Size, Index, Map) when Index == Size ->
    Map;
generate_map(Size, Index, Map) ->
    generate_map(Size, Index + 1, Map#{Index => Index}).

test_map_match(N, Map) ->
    [begin
        #{1 := V1} = Map,
        #{2 := V2} = Map,
        {V1, V2}
     end || _X <- lists:seq(1, N)].

test_maps_get_2(N, Map) ->
    [begin
         V1 = maps:get(1, Map),
         V2 = maps:get(2, Map),
         {V1, V2}
     end || _X <- lists:seq(1, N)].

test_maps_get_3(N, Map) ->
    [begin
         maps:get(1, Map, undefined),
         maps:get(not_found, Map, undefined)
     end || _X <- lists:seq(1, N)].


