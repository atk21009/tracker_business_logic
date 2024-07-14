-module(location_functions).
-export([get_location/2, create_location/2, change_location/4]).
-define(BUCKET, location_server:get_bucket()).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


get_location(Riak_Pid, LocationId) -> 
    try 
        {ok, Data} = database:get(Riak_Pid, ?BUCKET, LocationId),
        {ok, Data}
    catch
        error:Reason ->
            {error, Reason}
    end.

create_location(Riak_Pid, LocationId) ->
    try 
        BinaryKey = list_to_binary(LocationId),
        
        case database:get(Riak_Pid, ?BUCKET, LocationId) of 
            {ok, _} ->
                {ok, <<"Already Exists">>};
            {error, not_found} ->
                % Generate map object
                Location = [
                    {<<"latitude">>, <<>>},
                    {<<"longitude">>, <<>>}
                ],
                % place location in db
                database:put(Riak_Pid, ?BUCKET, BinaryKey, Location),
                {ok, LocationId}
        end
    catch
        error:Reason ->
            {error, Reason}
    end.

change_location(Riak_Pid, Location_id, Latitude, Longitude) ->
    try
        % Create unique Id
        BinaryKey = list_to_binary(Location_id),
        
        % Check if location id exists
        case database:get(Riak_Pid, ?BUCKET, BinaryKey) of
            {ok, Location} ->
                % Update the latitude and longitude values
                UpdatedLocation = lists:map(
                    fun
                        ({<<"latitude">>, _}) -> {<<"latitude">>, list_to_binary(Latitude)};
                        ({<<"longitude">>, _}) -> {<<"longitude">>, list_to_binary(Longitude)};
                        (Other) -> Other
                    end, 
                    Location
                ),
                
                % Store the updated location back in the database
                database:put(Riak_Pid, ?BUCKET, BinaryKey, UpdatedLocation),
                {ok, Location_id};
            {error, not_found} ->
                {error, location_not_found}
        end
    catch
        error:Reason ->
            {error, Reason}
    end.