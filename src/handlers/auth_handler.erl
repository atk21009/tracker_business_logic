-module(auth_handler).
-export([init/2]).

init(Req0, Opts) -> 
    Method = cowboy_req:method(Req0), % Get method
    Path = cowboy_req:path(Req0), % Get path
    {ok,Data,_} = cowboy_req:read_body(Req0), % Get body
    DecodedData = case Data of
        <<>> -> {};
        _ -> jsx:decode(Data)
    end,
    % Encode results
    Res = jsx:encode(auth_server:auth(Method, Path, DecodedData)), 
    % Response
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/json">>
    }, Res, Req0),
    {ok, Req, Opts}.
