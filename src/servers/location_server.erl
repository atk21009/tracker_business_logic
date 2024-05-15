%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
-module(location_server).
-behaviour(gen_server).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0,start/3,stop/0,location/3,get_bucket/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start(Registration_type,Name,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).
stop() -> gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% Other API
%%--------------------------------------------------------------------
location(<<"POST">>, <<"/location/update">>, []) ->
    ok;

location(<<"POST">>, <<"/location">>, #{<<"location_id">>:=LocationId,<<"latitude">>:=Latitude,<<"longitude">>:=Longitude}) ->
    gen_server:call(?MODULE, {new, {LocationId, Latitude, Longitude}});

location(<<"POST">>, <<"/location/keys">>, _) ->
    gen_server:call(?MODULE, {all}).

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
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init([]) ->
    database:start_link().
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% -spec handle_call(Request::term(), From::pid(), State::term()) ->
% {reply, term(), term()} |
% {reply, term(), term(), integer()} |
% {noreply, term()} |
% {noreply, term(), integer()} |
% {stop, term(), term(), integer()} | 
% {stop, term(), term()}.
%% @end
%%--------------------------------------------------------------------

handle_call({new, {LocationId, Latitude, Longitude}}, _From, Riak_Pid) ->
    try 
        {ok} = location_functions:create_location(Riak_Pid, LocationId, Latitude, Longitude),
        {reply, {ok}, Riak_Pid}
    catch
        error:Reason ->
            {reply, {fail, Reason}, Riak_Pid}
    end;

handle_call({rand_uuid}, _From, Riak_Pid) ->
    LocationId = uuid:to_string(uuid:uuid1()),
    LocationIdString = #{<<"Location_id">> => list_to_binary(LocationId)},
    {reply, LocationIdString, Riak_Pid};

                                
handle_call(stop, _From, _State) ->
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