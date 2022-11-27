%%%-------------------------------------------------------------------
%% @doc erlang_optimizer public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_optimizer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_optimizer_sup:start_link().

stop(_State) ->
    ok.




%% internal functions
