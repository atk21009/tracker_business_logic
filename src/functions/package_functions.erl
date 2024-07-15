-module(package_functions).

-export([create_package/1,put_package/3]).
-export([get_package/2, get_package_location_id/1]).
-export([package_delivered/1]).
-export([get_all_items/2]).

-define(BUCKET, package_server:get_bucket()).

%%--------------------------------------------------------------------
%% Create package - Completed
%%--------------------------------------------------------------------
put_package(Riak_Pid, PackageId, Package) ->
    try 
        database:put(Riak_Pid, ?BUCKET, PackageId, Package),
        {ok}
    catch
        error:Reason ->
            {error, Reason}
    end.
create_package(LocationId) ->
    try
        %% Get Local Time
        DateString = today_date(),
        %% Create package
        Package = #{
            <<"location_id">> => LocationId,
            <<"delivered">> => false,
            <<"created">> => list_to_binary(DateString)
        },
        {ok, Package}
    catch
        error:Reason ->
            io:format("Error in handle_call: ~p~n", [Reason]),
            {error, Reason}
    end.
%%--------------------------------------------------------------------
%% Get package - Completed
%%--------------------------------------------------------------------
get_package(Riak_Pid, PackageId) ->
    try 
        {ok, Data} = database:get(Riak_Pid, ?BUCKET, PackageId),
        {ok, Data}
    catch
        error:Reason ->
            {error, Reason}
    end.
get_package_location_id(Package) ->
    try
        LocationId = maps:get(<<"location_id">>, Package),
        {ok, LocationId}
    catch
        error:Reason ->
            {error, Reason}
    end.
%%--------------------------------------------------------------------
%% Deliver package - Completed
%%--------------------------------------------------------------------
package_delivered(Package) ->
    Created = maps:get(<<"created">>, Package),
    UpdatedPackage = #{
        <<"location_id">> => null,
        <<"delivered">> => true,
        <<"created">> => Created,
        <<"delivered_at">> => list_to_binary(today_date())
    },
    {ok, UpdatedPackage}.
%%--------------------------------------------------------------------
%% Get All - Completed
%%--------------------------------------------------------------------
get_all_items([], _) ->
    [];
get_all_items([Key | RestKeys], Pid) ->
    try 
        {ok, Item} = database:get(Pid, ?BUCKET, Key), 
        {ok, [#{Key => Item} | get_all_items(RestKeys, Pid)]}
    catch
        error:Reason ->
            {error, Reason}
    end.
%%--------------------------------------------------------------------
%% Transfer package - TODO
%%--------------------------------------------------------------------    

today_date() ->
    {{Year, Month, Day}, _} = calendar:local_time(),
    DateString = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day])),
    DateString.