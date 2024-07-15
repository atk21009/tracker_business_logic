-module(package_test).
-include_lib("eunit/include/eunit.hrl").

-ifdef(EUNIT).

package_server_test() ->
    % Start server
    package_server:start(),

    % Create Package Tests - Successful
    ?assertEqual(ok, package_transfer(#{<<"package_id">> => <<"1234">>, <<"location_id">> => <<"123456">>})),
    ?assertEqual(ok, package_transfer(#{<<"package_id">> => <<"567890">>, <<"location_id">> => <<"12512363">>})),
    % Create Package Tests - Edge Cases
    ?assertEqual(fail, package_transfer(#{<<"package_id">> => <<"1234">>})),
    ?assertEqual(fail, package_transfer(#{<<"package_id">> => <<"1234">>})),
    ?assertEqual(fail, package_transfer(#{<<"package_id">> => <<"1234">>})),

    % Update Location
    ?assertEqual(ok, location_update(#{<<"location_id">> =><<"123456">>, <<"latitude">>=><<"123456789">>, <<"longitude">> =><<"987654321">>})),
    ?assertEqual(ok, location_update(#{<<"location_id">> =><<"12512363">>, <<"latitude">>=><<"987654321">>, <<"longitude">> =><<"123456789">>})),
    % Update location - edge case
    ?assertEqual(fail, location_update(#{<<"location_id">> =><<"12512363">>, <<"latitude">>=><<"987654321">>})),
    ?assertEqual(fail, location_update(#{<<"location_id">> =><<"12512363">>,<<"longitude">> =><<"123456789">>})),
    ?assertEqual(fail, location_update(#{<<"location_id">> =><<"12512363">>})),


    % Get Package Tests - Successful
    ?assertEqual(ok, location_request(#{<<"package_id">> => <<"1234">>})),
    ?assertEqual(ok, location_request(#{<<"package_id">> => <<"567890">>})),
    % Get Package Tests - Edge Cases
    ?assertEqual(fail, location_request(#{<<"package_id">> => <<"">>})),
    ?assertEqual(fail, location_request(#{})),


    % Transfer Package - Successful
    ?assertEqual(ok, package_transfer(#{<<"package_id">> => <<"1234">>, <<"location_id">> => <<"12512363">>})),
    ?assertEqual(ok, package_transfer(#{<<"package_id">> => <<"567890">>, <<"location_id">> => <<"123456">>})),
    
    % Transfer Package - Edge Cases
    ?assertEqual(fail, package_transfer(#{<<"package_id">> => <<"1234">>})),
    ?assertEqual(fail, package_transfer(#{<<"location_id">> => <<"12345678">>})),

    % % Deliver Package - Successful
    ?assertEqual(ok, deliver_package_test(#{<<"package_id">> => <<"1234">>})),
    ?assertEqual(ok, deliver_package_test(#{<<"package_id">> => <<"567890">>})),
    % % Deliver Package - Edge Cases
    ?assertEqual(fail, deliver_package_test(#{<<"package_id">> => <<"111221321312">>})),
    ?assertEqual(fail, deliver_package_test(#{})),

    % Stop server
    package_server:stop().

%%--------------------------------------------------------------------
%% Create package - test
%%--------------------------------------------------------------------
package_transfer(PLL) ->    
    case package_server:package(<<"/package_transferred">>, PLL) of 
        {ok} ->
            ok;
        {fail, _} ->
            fail
    end.
%%--------------------------------------------------------------------
%% Get package - test
%%--------------------------------------------------------------------
location_request(Package_id) ->
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
        {ok} -> 
            ok;
        {fail, _} ->
            fail
    end.

%%--------------------------------------------------------------------
%% Transfer package - test
%%--------------------------------------------------------------------
location_update(Transfer_data) ->
    case package_server:package(<<"/location_update">>, Transfer_data) of
        {ok} ->
            ok;
        {fail, _} ->
            fail
    end.


-endif.
