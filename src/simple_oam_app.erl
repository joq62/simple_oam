%%%-------------------------------------------------------------------
%% @doc simple_oam public API
%% @end
%%%-------------------------------------------------------------------

-module(simple_oam_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    simple_oam_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
