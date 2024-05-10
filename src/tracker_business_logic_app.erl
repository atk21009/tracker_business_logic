%%%-------------------------------------------------------------------
%% @doc tracker_business_logic public API
%% @end
%%%-------------------------------------------------------------------

-module(tracker_business_logic_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            %% Hello World
            {"/", index_handler, []},
            %% Package Info
            {"/package/[...]", package_handler, []}
        ]}
    ]),
    % URL http://localhost:8080
    {ok,_} = cowboy:start_clear(
        http,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}),
    tracker_business_logic_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
