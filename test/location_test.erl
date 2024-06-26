-module(location_test).
-include_lib("eunit/include/eunit.hrl").

-ifdef(EUNIT).

location_server_test() ->
    location_server:start(),

    %% EUNIT Tests
    %% Update Location - Success
    location_server:location(<<"POST">>, <<"/location/update">>, #{<<"location_id">>=><<"123456">>,<<"latitude">>=><<"123344">>,<<"longitude">>=><<"67890">>}),
    %% Update Location - Edge Cases
    %% Get Location - Success 
    %% Get Location - Edge Cases
    %% Get All - Success (No edge)

    location_server:stop(),
    ok.

-endif.