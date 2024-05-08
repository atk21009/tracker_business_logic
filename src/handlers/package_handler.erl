-module(package_handler).

-export([init/2]).

init(Req0, Opts) -> 
    Method = cowboy_req:method(Req0), % [GET, POST, ...] 
    Path = cowboy_req:path(Req0), % Path of url
    {ok,Data,_} = cowboy_req:read_body(Req0), % Body if any
    DecodedData = case Data of
        <<>> -> {}; % pass empty tuple
        _ -> jsx:decode(Data) % decode data
    end,

    % pass to server
    Res = jsx:encode(package_server:package(Method, Path, DecodedData)),
    
    % send response to user
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/json">>
    }, Res, Req0),
    {ok, Req, Opts}.