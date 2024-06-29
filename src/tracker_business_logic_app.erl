%%%-------------------------------------------------------------------
%% @doc tracker_business_logic public API
%% @end
%%%-------------------------------------------------------------------

-module(tracker_business_logic_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tracker_business_logic_sup:start_link(),
    erpc:start([{name, ?MODULE}, {server, true}]).

stop(_State) ->
    ok.

%% internal functions
