-module(database).
-export([start/0]).
-export([put/4]).
-export([get/3, get_all/2]).
-export([delete/3]).
-export([clear_bucket/2]).

% Start link to db
start() -> 
    io:format("~nStarting database~n"),
    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    io:format("Database Started~n"),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% Add items
%%--------------------------------------------------------------------
put(Pid, Bucket, Key, Value) ->
    try
        JsonValue = jsx:encode(Value),
        Request = riakc_obj:new(Bucket, Key, JsonValue),
        riakc_pb_socket:put(Pid, Request),
        {ok}
    catch
        error:Reason ->
            io:format("Error in putting from database: ~p~n", [Reason]),
            {error, Reason}
    end.
%%--------------------------------------------------------------------
%% Get item using key
%%--------------------------------------------------------------------
get(Pid, Bucket, Key) ->
    case riakc_pb_socket:get(Pid, Bucket, Key) of
        {ok, RiakObject} ->
            Value = riakc_obj:get_value(RiakObject),
            Decoded = jsx:decode(Value),
            {ok, Decoded};
        {error, Reason} ->
            io:format("Error in getting from database: ~p~n", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Delete item using key
%%--------------------------------------------------------------------
delete(Pid, Bucket, Key) ->
    case riakc_pb_socket:delete(Pid, Bucket, Key) of
        ok ->
            io:format("Item with Key ~p removed successfully from Bucket ~p~n", [Key, Bucket]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.
%%--------------------------------------------------------------------
%% Get all items from bucket
%%--------------------------------------------------------------------
get_all(Pid, Bucket) ->
    try
        {ok, Keys} = riakc_pb_socket:list_keys(Pid, Bucket),
        {ok, Keys}
    catch
        error:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Clear Bucket
%%--------------------------------------------------------------------
clear_bucket(Pid,Bucket) ->
    try
        ok = riakc_pb_socket:delete_bucket(Pid, Bucket),
        {ok}
    catch
        error:Reason ->
            {error, Reason}
    end.
