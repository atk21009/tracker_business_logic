-module(location_functions).
-export([get_location/2, create_location/3, create_location/4, change_location/3]).
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

create_location(Riak_Pid, Latitude, Longitude) ->
    try 
            % Create unique Id
        LocationId = uuid:to_string(uuid:uuid1()),
        BinaryKey = list_to_binary(LocationId),
        % Generate map object
        Location = [
            {<<"latitude">>, <<Latitude/binary>>},
            {<<"longitude">>, <<Longitude/binary>>}
        ],
        % place location in db
        database:put(Riak_Pid, ?BUCKET, BinaryKey, Location),
        {ok, LocationId}
    catch
        error:Reason ->
            {error, Reason}
    end.

create_location(Riak_Pid, LocationId, Latitude, Longitude) ->
    try 
        Location = [
            {<<"latitude">>, <<Latitude/binary>>},
            {<<"longitude">>, <<Longitude/binary>>}
        ],
        {ok} = database:put(Riak_Pid, ?BUCKET, LocationId, Location),
        {ok}
    catch
        error:Reason ->
            {error, Reason}
    end.

change_location(Riak_Pid, Location_id, Package) ->
    try
        % Check if location id exists
        {ok, _} = database:get(Riak_Pid, ?BUCKET, Location_id),
        % Get package details and ensure not delivered
        false = maps:get(<<"delivered">>, Package),
        Created = maps:get(<<"created">>, Package),    
        % Update package location_id
        Updated = #{
            <<"location_id">> => Location_id,
            <<"delivered">> => false,
            <<"created">> => Created
        },
        % return updated
        {ok, Updated}
    catch
        error:Reason ->
            {error, Reason}
    end.