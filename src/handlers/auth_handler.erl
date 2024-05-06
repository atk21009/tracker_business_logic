-module(auth_handler).
-export([init/2]).

init(Req0, Opts) -> 
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    {ok,Data,_} = cowboy_req:read_body(Req0),
    DecodedData = case Data of
        <<>> -> % Empty data
            {};
        _ ->
            jsx:decode(Data)
    end,
    Res = jsx:encode(auth_server:auth(Method, Path, DecodedData)),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/json">>
    }, Res, Req0),
    {ok, Req, Opts}.
