-module(erlang_optimizer_reader).

%% API
-export([read_dir/1, read_file/1]).
-include("../include/print.hrl").

-record(state, {filename            :: string(),
                line_number = 1     :: integer(),
                calculation         :: map(),
                debug = true        :: boolean()
                }
        ).


read_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            read_all_erl_files(Dir, Files);
        Error ->
            Error
    end.


read_file(File) ->
    ?print("Analyzing %e~n", [File]),
    State = #state{filename = File, calculation = erlang_optimizer_calculations:calculate()},
    case file:open(File, [read]) of
        {ok, IO} ->
            read_all_lines(State, IO);
        Error ->
            Error
    end.


read_all_lines(State = #state{line_number = LN}, IO) ->
    case file:read_line(IO) of
        eof  -> ok;
        {ok, Data} ->
            analyze(State, Data),
            read_all_lines(State#state{line_number = LN + 1}, IO);
        Error ->
            Error
    end.

read_all_erl_files(_Dir, []) ->
    ok;
read_all_erl_files(Dir, [File | Tail]) ->
    case re:run(File, ".*\\.erl") of
        {match, _} ->
            read_file(Dir ++ "/" ++ File),
            read_all_erl_files(Dir, Tail);
        _ ->
            read_all_erl_files(Dir, Tail)
    end.


analyze(#state{filename = FN, line_number = LN, calculation = Calculation}, DataA) ->
    %% maps:get optimization
    case re:run(DataA,"maps:get\\((.*)\\)", []) of
        {match,[{Start, Length}, {StartG1, LengthG1}]} ->
            MapFunc = string:substr(DataA, Start + 1, Length),
            MapArgsStr = string:substr(DataA, StartG1 + 1, LengthG1),
            MapArgs = string:tokens(MapArgsStr, ","),
            Format = "~s:~p %y~n~s~n",
            case MapArgs of
                [Key, Map] ->
                    Name = map_match_vs_get_2,
                    #{Name := {_, Val}} = Calculation,
                    ?print(Format, [FN, LN, MapFunc, help(Name)]),
                    ?print("Using \"#{~s := Val} = ~s\" may decrease cpu usage for that call on %g ~n~n",
                        [Key, Map, Val]);
                [Key, Map, _Default] ->
                    Name = get_2_vs_get_3,
                    ?print(Format, [FN, LN, MapFunc, help(Name)]),
                    #{Name := {_, Val}} = Calculation,
                    ?print("Using \"maps:get(~s, ~s)\" may decrease cpu usage for that call on %g ~n~n",
                        [Key, Map, Val]);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.


help(map_match_vs_get_2) ->
    "    If the map is small and the keys are constants known at compile-time,
    using the map matching syntax will be more efficient than multiple calls to maps:get/2.";
help(get_2_vs_get_3) ->
    "    A call maps:get/3 is more expensive than a call to maps:get/2.
    If a small map is used as alternative to using a record, instead of calling maps:get/3
    multiple times to handle default values, consider putting the default values in a map and merging that map with the other map";
help(_) ->
    "".