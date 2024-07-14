-module(package_test).
-include_lib("eunit/include/eunit.hrl").

-ifdef(EUNIT).

package_server_test() ->
    % Start server
    package_server:start(),

    Auth = env_variables:auth_key(),
    ?assertEqual({ok}, package_server:package(<<"/package/clear">>, #{<<"auth">> => Auth})),

    % Create Package Tests - Successful
    ?assertEqual(ok, create_package_test(#{<<"package_id">> => <<"1234">>, <<"latitude">> => <<"123456">>, <<"longitude">> => <<"67890">>})),
    ?assertEqual(ok, create_package_test(#{<<"package_id">> => <<"567890">>, <<"latitude">> => <<"12512363">>, <<"longitude">> => <<"26745724">>})),
    % Create Package Tests - Edge Cases
    ?assertEqual(fail, create_package_test(#{<<"package_id">> => <<"1234">>, <<"latitude">> => <<"123456">>})),
    ?assertEqual(fail, create_package_test(#{<<"package_id">> => <<"1234">>,<<"longitude">> => <<"67890">>})),
    ?assertEqual(fail, create_package_test(#{<<"package_id">> => <<"1234">>})),

    % Get Package Tests - Successful
    ?assertEqual(ok, get_package_test(#{<<"package_id">> => <<"1234">>})),
    ?assertEqual(ok, get_package_test(#{<<"package_id">> => <<"567890">>})),
    % Get Package Tests - Edge Cases
    ?assertEqual(fail, get_package_test(#{<<"package_id">> => <<"1111111111">>})),
    ?assertEqual(fail, get_package_test(#{})),

    % Get Location Tests - Successful
    ?assertEqual(ok, get_package_location(#{<<"package_id">> => <<"1234">>})),
    ?assertEqual(ok, get_package_location(#{<<"package_id">> => <<"567890">>})),
    % Get Location Tests - Edge Cases
    ?assertEqual(fail, get_package_location(#{<<"package_id">> => <<"1111111111111">>})),
    ?assertEqual(fail, get_package_location(#{})),

    % Get all items and keys (No edge cases needed)
    ?assertEqual(ok, get_all_keys()),
    ?assertEqual(ok, get_all_items()),

    Location_id_1 = get_location_id(#{<<"package_id">> => <<"1234">>}),
    Location_id_2 = get_location_id(#{<<"package_id">> => <<"567890">>}),

    % Transfer Package - Successful
    ?assertEqual(ok, transfer_package(#{<<"package_id">> => <<"1234">>, <<"location_id">> => Location_id_2})),
    ?assertEqual(ok, transfer_package(#{<<"package_id">> => <<"567890">>, <<"location_id">> => Location_id_1})),
    
    % Transfer Package - Edge Cases
    ?assertEqual(fail, transfer_package(#{<<"package_id">> => <<"1234">>, <<"location_id">> => <<"12345678">>})),
    ?assertEqual(fail, transfer_package(#{<<"package_id">> => <<"2315346">>, <<"location_id">> => <<"12345678">>})),

    % Deliver Package - Successful
    ?assertEqual(ok, deliver_package_test(#{<<"package_id">> => <<"1234">>})),
    ?assertEqual(ok, deliver_package_test(#{<<"package_id">> => <<"567890">>})),
    % Deliver Package - Edge Cases
    ?assertEqual(fail, deliver_package_test(#{<<"package_id">> => <<"111221321312">>})),
    ?assertEqual(fail, deliver_package_test(#{})),

    % Stop server
    package_server:stop().

%%--------------------------------------------------------------------
%% Create package - test
%%--------------------------------------------------------------------
create_package_test(PLL) ->    
    case package_server:package(<<"/package/create">>, PLL) of 
        {ok, _} ->
            ok;
        {fail, _} ->
            fail
    end.
%%--------------------------------------------------------------------
%% Get package - test
%%--------------------------------------------------------------------
get_package_test(Package_id) ->
    case package_server:package(<<"/package">>, Package_id) of 
        {ok,_} ->
            ok;
        {fail, _} ->
            fail
    end.
%%--------------------------------------------------------------------
%% Deliver package - test
%%--------------------------------------------------------------------
deliver_package_test(Package_id) ->
    case package_server:package(<<"/package/delivered">>, Package_id) of
        {ok, _} -> 
            ok;
        {fail, _} ->
            fail
    end.
%%--------------------------------------------------------------------
%% Get package location - test
%%--------------------------------------------------------------------
get_package_location(Package_id) ->
    case package_server:package(<<"/package/location">>, Package_id) of
        {ok,_} ->
            ok;
        {fail, _} ->
            fail
    end.
%%--------------------------------------------------------------------
%% Get all - test
%%--------------------------------------------------------------------
get_all_items() ->
    case package_server:package(<<"/package/all">>, {}) of
        {ok, _} ->
            ok;
        {fail, _} ->
            fail
    end.
%%--------------------------------------------------------------------
%% Get all keys - test
%%--------------------------------------------------------------------
get_all_keys() ->
    case package_server:package(<<"/package/keys">>, {}) of
        {ok, _} ->
            ok;
        {fail, _} ->
            fail
    end.
%%--------------------------------------------------------------------
%% Transfer package - test
%%--------------------------------------------------------------------
transfer_package(Transfer_data) ->
    case package_server:package(<<"/package/transfer">>, Transfer_data) of
        {ok, _} ->
            ok;
        {fail, _} ->
            fail
    end.
%%--------------------------------------------------------------------
%% Clear Database - test
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------
get_location_id(Package_id) ->
    {ok, ResponseMap} = package_server:package(<<"/package">>, Package_id),
    LocationData = maps:get(<<"Package Data">>, ResponseMap),
    Location_id = maps:get(<<"location_id">>, LocationData),
    Location_id.

-endif.
