-module(database).
-export([start_link/0, put/4, get/3, get_all/2, delete/3]).

% Start link to db
start_link() -> 
    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    {ok, Pid}.

% Add items
put(Pid, Bucket, Key, Value) -> 
    Request = riakc_obj:new(Bucket, Key, Value),
    riakc_pb_socket:put(Pid, Request).

% Get item using key
get(Pid, Bucket, Key) ->
    case riakc_pb_socket:get(Pid, Bucket, Key) of
        {ok, Fetched} ->
            binary_to_term(riakc_obj:get_value(Fetched));
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

% Get all items
get_all(Pid, Bucket) -> 
    Keys = riakc_pb_socket:list_keys(Pid, Bucket),
    [get_and_encode(Bucket, Pid, Key) || Key <- Keys].


% Get and jsx encode
get_and_encode(Bucket,Pid,Key) ->
    {ok, {_, Res_Key, Res_Value}} = riakc_pb_socket:get(Pid, Bucket, Key),
    jsx:encode([{Res_Key, Res_Value}]).

% Delete item using key
delete(Pid, Bucket, Key) ->
    case riakc_pb_socket:delete(Pid, Bucket, Key) of
        ok ->
            io:format("Item with Key ~p removed successfully from Bucket ~p~n", [Key, Bucket]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.