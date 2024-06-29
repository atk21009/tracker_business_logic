-module(package_server).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0,start/3,stop/0,package/3,get_bucket/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(BUCKET, get_bucket()).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    io:format("Starting package_server~n"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start(Registration_type,Name,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).
stop() -> gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% Package details API
%% %%--------------------------------------------------------------------
package(<<"POST">>, <<"/package">>, #{<<"package_id">> := PackageId}) -> 
    gen_server:call(?MODULE, {get_package, PackageId});

%% Create package
package(<<"POST">>, <<"/package/create">>, #{<<"package_id">> := PackageId, <<"latitude">> := Latitude, <<"longitude">> := Longitude}) ->
    gen_server:call(?MODULE, {create_package, {PackageId, Latitude, Longitude}});

%% Package Delivered
package(<<"POST">>, <<"/package/delivered">>, #{<<"package_id">> := PackageId}) -> 
    gen_server:call(?MODULE, {package_delivered, PackageId});

%% Request Location
package(<<"POST">>, <<"/package/location">>, #{<<"package_id">> := PackageId}) ->
    gen_server:call(?MODULE, {get_package_location, PackageId});

%% Package Transfer
package(<<"POST">>, <<"/package/transfer">>, #{<<"package_id">> := PackageId, <<"location_id">> := LocationId}) ->
    gen_server:call(?MODULE, {package_transfer, {PackageId, LocationId}});

%% Get all keys and items
package(<<"POST">>, <<"/package/all">>, {}) ->
    gen_server:call(?MODULE, {get_all});

%% Get all keys
package(<<"POST">>, <<"/package/keys">>, {}) ->
    gen_server:call(?MODULE, {get_all_keys});

package(<<"POST">>, <<"/package/clear">>, #{<<"auth">>:=Auth_key}) ->
    gen_server:call(?MODULE, {clear, {Auth_key}});

package(<<"POST">>, <<"/package/test">>, #{}) ->
    gen_server:call(?MODULE, {test});

%% Not found
package(_, _, _) -> 
    gen_server:call(?MODULE, {error}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init([]) ->
    database:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call({test}, _From, Riak_Pid) -> 
    {reply, {ok, <<"IT WORKS!">>}, Riak_Pid};

handle_call({create_package, {PackageId, Latitude, Longitude}}, _From, Riak_Pid) ->
    try 
        {ok, LocationId} = location_functions:create_location(Riak_Pid, Latitude, Longitude),
        {ok, Package} = package_functions:create_package(LocationId),
        {ok} = package_functions:put_package(Riak_Pid, PackageId, Package),
        {reply, {ok, [PackageId, Package]}, Riak_Pid}
    catch
        error:Reason ->
            {reply, {fail, Reason}, Riak_Pid}
    end;

handle_call({get_package, PackageId}, _From, Riak_Pid) -> 
    try 
        {ok, Package_Data} = package_functions:get_package(Riak_Pid, PackageId),
        {ok, LocationId} = package_functions:get_package_location_id(Package_Data),
        {ok, Location_Data} = location_functions:get_location(Riak_Pid, LocationId),
        Response = #{<<"Package Data">> => Package_Data, <<"Location Data">> => Location_Data},
        {reply, {ok, Response}, Riak_Pid}
    catch
        error:Reason ->
            {reply, {fail, Reason}, Riak_Pid}
    end;

handle_call({package_delivered, PackageId}, _From, Riak_Pid) -> 
    try 
        {ok, Package} = package_functions:get_package(Riak_Pid, PackageId),
        {ok, Updated_package} = package_functions:package_delivered(Package),
        {ok} = package_functions:put_package(Riak_Pid, PackageId, Updated_package),
        {reply, {ok, Updated_package}, Riak_Pid}
    catch
        error:Reason ->
            {reply, {fail, Reason}, Riak_Pid}
    end;

handle_call({get_package_location, PackageId}, _From, Riak_Pid) ->
    try 
        {ok, Package} = package_functions:get_package(Riak_Pid, PackageId),
        {ok, LocationId} = package_functions:get_package_location_id(Package),
        {ok, Location_Data} = location_functions:get_location(Riak_Pid, LocationId),
        {reply, {ok, Location_Data}, Riak_Pid}
    catch
        error:Reason ->
            {reply, {fail, Reason}, Riak_Pid}
    end;

handle_call({get_all}, _From, Riak_Pid) ->
    try 
        {ok, Keys} = database:get_all(Riak_Pid, ?BUCKET),
        {ok, All} = package_functions:get_all_items(Keys, Riak_Pid),
        {reply, {ok, All}, Riak_Pid}
    catch
        error:Reason ->
            {reply, {fail, Reason}, Riak_Pid}
    end;

handle_call({get_all_keys}, _From, Riak_Pid) ->
    try 
        {ok, Keys} = database:get_all(Riak_Pid, ?BUCKET),
        {reply, {ok, Keys}, Riak_Pid}
    catch
        error:Reason ->
            {reply, {fail, Reason}, Riak_Pid}
    end;

%% TODO: Implement transfer logic
handle_call({package_transfer, {Package_id, Location_id}}, _From, Riak_Pid) ->
    try 
        %% Get package 
        {ok, Package} = package_functions:get_package(Riak_Pid, Package_id),
        %% Get updated location
        {ok, Updated} = location_functions:change_location(Riak_Pid, Location_id, Package),
        %% If successful update package
        {ok} = package_functions:put_package(Riak_Pid, Package_id, Updated),
        {reply, {ok}, Riak_Pid}
    catch
        error:Reason ->
            {reply, {fail, Reason}, Riak_Pid}
    end;

handle_call({clear, {Auth_key}}, _From, Riak_Pid) ->
    case auth_key:check_auth_key(Auth_key) of
        true ->
            database:clear_bucket(Riak_Pid, ?BUCKET),
            Location_bucket = location_server:get_bucket(),
            database:clear_bucket(Riak_Pid, Location_bucket),
            {reply, {ok}, Riak_Pid};
        _ ->
            {reply, {fail}, Riak_Pid}
    end;

handle_call({error}, _From, Riak_Pid) ->
    {reply, {fail, <<"Invalid Request">>}, Riak_Pid};
    

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
get_bucket() ->
    <<"package-test">>.
-else.
get_bucket() ->
    <<"package">>.
-endif.