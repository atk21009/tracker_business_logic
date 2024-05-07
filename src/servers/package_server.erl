-module(package_server).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0,start/3,stop/0,package/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server assuming there is only one server started for 
%% this module. The server is registered locally with the registered
%% name being the name of the module.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%% Args is a list containing any data to be passed to the gen_server's
%% init function.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).

%% Package details API
package(<<"GET">>, <<"/package">>, #{<<"id">> := Id}) -> 
    gen_server:call(?MODULE, {get_package, Id});
%% Create package
package(<<"POST">>, <<"/package/create">>, #{<<"id">> := Id, <<"latitude">> := Latitude, <<"longitude">> := Longitude}) ->
    gen_server:call(?MODULE, {create_package, {Id, Latitude, Longitude}});
%% Package Delivered
package(<<"POST">>, <<"/package/status">>, #{<<"id">> := Id}) -> 
    gen_server:call(?MODULE, {package_delivered, Id});
%% Request Location
package(<<"GET">>, <<"/package/location">>, #{<<"id">> := Id}) ->
    gen_server:call(?MODULE, {get_package_location, Id});
%% Package Transfer
package(<<"POST">>, <<"/package/location">>, #{<<"id">> := Id, <<"latitude">> := Latitude, <<"longitude">> := Longitude}) ->
    gen_server:call(?MODULE, {package_transfer, {Id, Latitude, Longitude}});
%% Not found
package(_, _, _) -> 
    Req = cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"Bad Request">>),
    {ok, Req, undefined}.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
init([]) ->
    database:start_link().
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({get_package, Id}, _From, Riak_Pid) -> 
    Data = database:get(Riak_Pid, <<"package">>, Id), 
    {reply, Data, Riak_Pid};

handle_call({create_package, {Id, Latitude, Longitude}}, _From, Riak_Pid) ->
    io:format("Creating package with ID: ~p, Latitude: ~p, Longitude: ~p~n", [Id, Latitude, Longitude]),
    database:put(Riak_Pid, <<"package">>, Id, #{<<"Latitude">> => Latitude, <<"Longitude">> => Longitude}),
    {reply, [Id, Longitude, Latitude], Riak_Pid};

handle_call({package_delivered, Id}, _From, Riak_Pid) -> 
    database:delete(Riak_Pid, <<"package">>, Id),
    Msg = <<"Package ", Id/binary, " has been delivered">>,
    {reply, Msg, Riak_Pid};

handle_call({get_package_location, Id}, _From, Riak_Pid) ->
    Data = database:get(Riak_Pid, <<"package">>, Id),
    {reply, Data, Riak_Pid};

handle_call({package_transfer, {Id, Latitude, Longitude}}, _From, Riak_Pid) ->
    package_transfer_event:call(package_transfer),
    io:format("ID: ~p~nLatitude: ~p~nLongitude: ~p~n", [Id, Latitude, Longitude]),
    {reply, [Id, Longitude, Latitude], Riak_Pid};

handle_call(stop, _From, _OldVsnRiak_Pid) ->
        {stop,normal,
                replace_stopped,
          down}. %% setting the server's internal state to down

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::term()) -> {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term()}.
handle_cast(_Msg, State) ->
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info::term(), State::term()) -> {noreply, term()} |
                                   {noreply, term(), integer()} |
                                   {stop, term(), term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), term()) -> term().
terminate(_Reason, _State) ->
    ok.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================



-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
-endif.