-module(package_test).
-include_lib("eunit/include/eunit.hrl").

-ifdef(EUNIT).

package_server_test() ->
    % Start server
    package_server:start(),

    Auth = env_variables:auth_key(),
    ?assertEqual({ok}, package_server:package(<<"/package/clear">>, #{<<"auth">> => Auth})),

    % Create Package Tests - Successful
    ?assertEqual(ok, create_package_test(#{<<"package_id">> => <<"1234">>, <<"location_id">> => <<"123456">>})),
    ?assertEqual(ok, create_package_test(#{<<"package_id">> => <<"567890">>, <<"location_id">> => <<"12512363">>})),
    % Create Package Tests - Edge Cases
    ?assertEqual(fail, create_package_test(#{<<"package_id">> => <<"1234">>, <<"latitude">> => <<"123456">>})),
    ?assertEqual(fail, create_package_test(#{<<"package_id">> => <<"1234">>,<<"longitude">> => <<"67890">>})),
    ?assertEqual(fail, create_package_test(#{<<"package_id">> => <<"1234">>})),

    % Get Package Tests - Successful
    ?assertEqual(ok, get_package_test(#{<<"package_id">> => <<"1234">>})),
    ?assertEqual(ok, get_package_test(#{<<"package_id">> => <<"567890">>})),
    % Get Package Tests - Edge Cases
    ?assertEqual(fail, get_package_test(#{<<"package_id">> => <<"">>})),
    ?assertEqual(fail, get_package_test(#{})),


    % Transfer Package - Successful
    ?assertEqual(ok, transfer_package(#{<<"package_id">> => <<"1234">>, <<"location_id">> => <<"12512363">>})),
    ?assertEqual(ok, transfer_package(#{<<"package_id">> => <<"567890">>, <<"location_id">> => <<"123456">>})),
    
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
    case package_server:package(<<"/package_transferred">>, PLL) of 
        {ok, _} ->
            ok;
        {fail, _} ->
            fail
    end.
%%--------------------------------------------------------------------
%% Get package - test
%%--------------------------------------------------------------------
get_package_test(Package_id) ->
    case package_server:package(<<"/location_request">>, Package_id) of 
        {ok,_} ->
            ok;
        {fail, _} ->
            fail
    end.
%%--------------------------------------------------------------------
%% Deliver package - test
%%--------------------------------------------------------------------
deliver_package_test(Package_id) ->
    case package_server:package(<<"/delivered">>, Package_id) of
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
        {ok} ->
            ok;
        {fail, _} ->
            fail
    end.


-endif.
