-module(erlang_optimizer_reader).

%% API
-export([read_dir/1, read_file/1]).
-include("../include/print.hrl").

-record(state, {filename            :: string(),
                line_number = 1     :: integer(),
                calculation         :: map(),
                debug = true        :: boolean(),
                function = undefined :: string() | undefined
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
        {ok, Line} ->
            %% ignore comments and empty line
            NewState =
                case pre_process_line(Line) of
                    skip ->
                        State;
                    NewLine ->
                        State1 = check_fun(State, NewLine),
                        analyze(State1, NewLine),
                        State1
                end,
            read_all_lines(NewState#state{line_number = LN + 1}, IO);
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

-spec analyze(#state{}, string()) -> ok.
analyze(State, Line) ->
    analyze_maps(State, Line).


analyze_maps(State, Line) ->
    case re:run(Line,"maps:get\\((.*)\\)", [ungreedy, global]) of
        {match, MatchResult} ->
            match_all(State, Line, MatchResult);
        _ ->
            ok
    end.

match_all(_State, _StrData, []) ->
    ok;
match_all(#state{filename = FN, line_number = LN, calculation = Calculation, function = Func} = State, StrData, [[{Start, Length}, {StartG1, LengthG1}] | Rest]) ->
    MapFunc = string:substr(StrData, Start + 1, Length),
    MapArgsStr = string:substr(StrData, StartG1 + 1, LengthG1),
    MapArgs = string:tokens(MapArgsStr, ","),
    Format = "~s:~p in function: %e - %y~n~s~n",
    case MapArgs of
        [Key, Map] ->
            Name = map_match_vs_get_2,
            #{Name := {_, Val}} = Calculation,
            ?print(Format, [FN, LN, Func, MapFunc, help(Name)]),
            ?print("Using \"#{~s := Val} = ~s\" may decrease cpu usage for that call on %g ~n~n",
                [Key, Map, Val]);
        [Key, Map, _Default] ->
            Name = get_2_vs_get_3,
            ?print(Format, [FN, LN, Func, MapFunc, help(Name)]),
            #{Name := {_, Val}} = Calculation,
            ?print("Using \"maps:get(~s, ~s)\" may decrease cpu usage for that call on %g ~n~n",
                [Key, Map, Val]);
        _ ->
            ok
    end,
    match_all(State, StrData, Rest).


pre_process_line(Data) ->
    [NewLine | _] = string:split(Data, "%%"),
    case re:run(NewLine,"\\S") of
        {match, _} ->
            NewLine;
        _ ->
            skip
    end.

check_fun(State, Line) ->
    case re:run(Line,"^\\w.*\\(.*\\)", [ungreedy]) of
        {match, [{Start, Length}]} ->
            State#state{function = string:substr(Line, Start + 1, Length)};
        _ ->
            State
    end.

help(map_match_vs_get_2) ->
    "    If the map is small and the keys are constants known at compile-time,
    using the map matching syntax will be more efficient than multiple calls to maps:get/2.";
help(get_2_vs_get_3) ->
    "    A call maps:get/3 is more expensive than a call to maps:get/2.
    If a small map is used as alternative to using a record, instead of calling maps:get/3
    multiple times to handle default values, consider putting the default values in a map and merging that map with the other map".