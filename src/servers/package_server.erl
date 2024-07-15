-module(package_server).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0, start/3, stop/0, package/2, get_bucket/0, get_location_bucket/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(BUCKET, get_bucket()).


%%%===================================================================
%%% API
%%%===================================================================
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(Registration_type, Name, Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).

stop() -> gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% Package details API
%%--------------------------------------------------------------------
package(<<"/package_transferred">>, #{<<"package_id">> := PackageId, <<"location_id">> := LocationId}) ->
    gen_server:call(?MODULE, {package_transfer, PackageId, LocationId});

package(<<"/delivered">>, #{<<"package_id">> := PackageId}) ->
    gen_server:call(?MODULE, {delivered, PackageId});

package(<<"/location_request">>, #{<<"package_id">> := PackageId}) ->
    gen_server:call(?MODULE, {location, PackageId});

package(<<"/location_update">>, #{<<"location_id">>:=LocationId, <<"latitude">>:=Latitude, <<"longitude">>:=Longitude}) ->
    gen_server:call(?MODULE, {update_location, {LocationId, Latitude, Longitude}});

package(<<"/package/test">>, #{}) ->
    gen_server:call(?MODULE, {test});

%% Not found
package(_, _) ->
    gen_server:call(?MODULE, {error}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init(term()) -> {ok, term()} | {ok, term(), number()} | ignore | {stop, term()}.
init([]) ->
    io:format("Package Server Started~n"),
    database:start().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call({package_transfer, PackageId, LocationId}, _From, Riak_Pid) ->
    try
        {ok, Package} = package_functions:create_package(LocationId),
        {ok} = package_functions:put_package(Riak_Pid, PackageId, Package),
        {reply, {ok}, Riak_Pid}
    catch
        error:Reason ->
            io:format("Error in create_package: ~p~n", [Reason]),
            {reply, {fail, Reason}, Riak_Pid}
    end;

handle_call({delivered, PackageId}, _From, Riak_Pid) ->
    try 
        {ok, Package} = package_functions:get_package(Riak_Pid, PackageId),
        {ok, Updated_package} = package_functions:package_delivered(Package),
        {ok} = package_functions:put_package(Riak_Pid, PackageId, Updated_package),
        {reply, {ok}, Riak_Pid}
    catch
        error:Reason ->
            {reply, {fail, Reason}, Riak_Pid}
    end;

handle_call({location, PackageId}, _From, Riak_Pid) ->
    try 
        {ok, Package} = package_functions:get_package(Riak_Pid, PackageId),
        {ok, LocationId} = package_functions:get_package_location_id(Package),
        {ok, Location_Data} = location_functions:get_location(Riak_Pid, LocationId),
        {reply, {ok, Location_Data}, Riak_Pid}
    catch
        error:Reason ->
            {reply, {fail, Reason}, Riak_Pid}
    end;

handle_call({update_location, {LocationId, Latitude, Longitude}}, _From, Riak_Pid) ->
    io:format("handle_call: update_location with LocationId: ~p, Latitude: ~p, Longitude: ~p~n", [LocationId, Latitude, Longitude]),
    try
        {ok} = location_functions:change_location(Riak_Pid, LocationId, Latitude, Longitude),
        {reply, {ok}, Riak_Pid}
    catch
        error:Reason ->
            io:format("Error in update location: ~p~n", [Reason]),
            {reply, {fail, Reason}, Riak_Pid}
    end;

handle_call({test}, _From, Riak_Pid) -> 
    {reply, {ok, <<"Package server works">>}, Riak_Pid};

handle_call({error}, _From, Riak_Pid) ->
    {reply, {fail, <<"Invalid request">>}, Riak_Pid};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

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
-spec terminate(Reason::term(), State::term()) -> term().
terminate(_Reason, _State) ->
    ok.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), State::term(), Extra::term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-ifdef(EUNIT).
get_bucket() ->
    <<"package-test">>.
get_location_bucket() ->
    <<"location-test">>.
-else.
get_bucket() ->
    <<"package">>.
get_location_bucket() ->
    <<"location">>.
-endif.