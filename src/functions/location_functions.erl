-module(location_functions).
-export([get_location/2, change_location/4]).
-define(BUCKET, package_server:get_location_bucket()).

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

change_location(Riak_Pid, Location_id, Latitude, Longitude) ->    
    io:format("change_location called with Location_id: ~p, Latitude: ~p, Longitude: ~p~n", [Location_id, Latitude, Longitude]),
    
    % Check if location id exists
    case database:get(Riak_Pid, ?BUCKET, Location_id) of
        {ok, _} ->
            % Update the latitude and longitude values
            UpdatedLocation = #{ 
                <<"latitude">> => Latitude,
                <<"longitude">> => Longitude
            },
            io:format("Updated location: ~p~n", [UpdatedLocation]),

            % Store the updated location back in the database
            case database:put(Riak_Pid, ?BUCKET, Location_id, UpdatedLocation) of
                {ok} -> 
                    io:format("Updated location stored successfully~n"),
                    {ok};
                {error, Reason} -> 
                    io:format("Error storing updated location: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, notfound} ->
            NewLocation = #{ 
                <<"latitude">> => Latitude,
                <<"longitude">> => Longitude
            },
            io:format("Creating new location: ~p~n", [NewLocation]),

            % Place new location in db
            case database:put(Riak_Pid, ?BUCKET, Location_id, NewLocation) of
                {ok} -> 
                    io:format("New location stored successfully~n"),
                    {ok};
                {error, Reason} -> 
                    io:format("Error storing new location: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Error retrieving location: ~p~n", [Reason]),
            {error, Reason}
    end.


