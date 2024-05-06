-module(auth_db).
-export([start_link/0, put/3, get/2, get_all/1]).


start_link() -> 
    {ok, Pid} = riakc_pd_socket:start_link("127.0.0.1", 8087),
    {ok, Pid}.

put(Bucket, Key, Value) -> 
    {ok, Pid} = start_link(),
    riakc_pb_socket:put(Pid, Bucket, Key, Value).

get(Bucket, Key) ->
    {ok, Pid} = start_link(),
    {ok, {_Bucket, _Key, Value}} = riakc_pb_socket:get(Pid, Bucket, Key),
    Value.

get_all(Bucket) -> 
    {ok, Pid} = start_link(),
    Keys = riakc_pb_socket:list_keys(Pid, Bucket),
    [get_and_encode(Bucket, Pid, Key) || Key <- Keys].

get_and_encode(Bucket,Pid,Key) ->
    {ok, {_, Res_Key, Res_Value}} = riakc_pb_socket:get(Pid, Bucket, Key),
    jsx:encode([{Res_Key, Res_Value}]).