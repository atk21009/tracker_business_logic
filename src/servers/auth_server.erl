%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
-module(auth_server).
-behaviour(gen_server).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0,start/3,stop/0,auth/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% State
-record(state, {
    objects :: map()
}).

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
%% @spec start() -> {ok, pid()} | ignore | {error, term()}.
%% 
%% @end
%%--------------------------------------------------------------------
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

%% Any other API functions go here.

%%--------------------------------------------------------------------
%% @doc
%% Handles Authentication
%%
%% @spec auth(Method :: atom(), Path :: atom(), Data :: atom()).
%%
%% @end
%%--------------------------------------------------------------------
%% Get User
auth(<<"GET">>, <<"/auth/all">>, Data) ->
    gen_server:call(?MODULE, {get_all_objects, Data});
auth(<<"GET">>, <<"/auth">>, #{<<"email">> := Email}) ->
    gen_server:call(?MODULE, {user, {Email}});
%% Register
auth(<<"POST">>, <<"/auth/register">>, #{<<"email">> := Email, <<"password">> := Password, <<"username">> := Username}) -> 
    gen_server:call(?MODULE, {register, {Username, Email, Password}});
%% Login
auth(<<"POST">>, <<"/auth/login">>, #{<<"email">> := Email, <<"password">> := Password}) ->
    gen_server:call(?MODULE, {login, {Email, Password}});
%% Update
auth(<<"PATCH">>, <<"/auth">>, _Data) ->
    Req = cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"Bad Request">>),
    {ok, Req, undefined};
%% Delete
auth(<<"DELETE">>, <<"/auth">>, #{<<"email">> := Email}) ->
    gen_server:call(?MODULE, {delete, {Email}});
%% Invalid request
auth(_, _, _) ->
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
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init(_) ->
    InitialState = #state{objects = #{}},
    {ok, InitialState};

init([]) ->
    {ok,replace_up}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::pid(), State::term()) ->
                                  {reply, term(), term()} |
                                  {reply, term(), term(), integer()} |
                                  {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term(), integer()} | 
                                  {stop, term(), term()}.
handle_call({get_all_objects, _}, _From, State) ->
    % Assuming State is your current state containing the state record
    _ = #state{objects = Objects} = State,
    % Encoding the state record into JSON
    JsonString = jsx:encode([{<<"objects">>, maps:to_list(Objects)}]),
    {reply, JsonString, State};
    
handle_call({user, {Email}}, _From, State) ->
    RecordMap = maps:get(Email, State, not_found),
    case RecordMap of
        not_found ->
            {reply, not_found, State};
        _ ->
            EncodedRecord = jsx:encode(map_to_record(Email, RecordMap)),
            {reply, EncodedRecord, State}
    end;

handle_call({register, {Username, Email, Password}}, _From, State) ->
    NewObject = {Username,Email,crypto:hash(md5, Password)},
    NewState = State#state{objects = maps:put(object_key(NewObject), NewObject, State#state.objects)},
    {reply, ok, NewState};

handle_call({login, {Email, Password}}, _From, State) -> 
    case find_user_by_email(Email, State) of
        {ok, User} ->
            case verify_password(Password, User) of
                true -> {reply, {ok, User}, State};
                false -> {reply, {error, invalid_password}, State}
            end;
        not_found -> {reply, {error, user_not_found}, State}
    end;

handle_call({update, {Email, NewData}}, _From, State) ->
    case find_user_by_email(Email, State) of
        {ok, _} ->
            UpdatedObject = NewData, % You should validate and merge the new data with the existing user data here
            UpdatedState = State#state{objects = maps:put(object_key(UpdatedObject), UpdatedObject, State#state.objects)},
            {reply, ok, UpdatedState};
        not_found -> {reply, {error, user_not_found}, State}
    end;
    
handle_call({delete, {Email}}, _From, State) ->
    case find_user_by_email(Email, State) of
        {ok, _} ->
            NewObjects = maps:remove(Email, State#state.objects),
            UpdatedState = State#state{objects = NewObjects},
            {reply, ok, UpdatedState};
        not_found -> {reply, {error, user_not_found}, State}
    end;

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

%% Helper function to find a user by username
find_user_by_email(Email, State) ->
    lists:keyfind(Email, 1, maps:values(State#state.objects)).

%% Helper function to verify password
verify_password(Password, {_, _, StoredPassword}) ->
    crypto:hash(md5, Password) =:= StoredPassword.

%% This function generates a unique key for a user object based on its email.
object_key({_Username, Email, _Password}) ->
    Email.

map_to_record(Email, RecordMap) ->
    % Extract the objects map from RecordMap
    Objects = maps:get(objects, RecordMap),
    
    % Extract the record corresponding to the given email from the objects map
    {Name, Email, Hash} = maps:get(Email, Objects),
    
    % Encode the record into JSON
    JsonString = jsx:encode([{<<"name">>, Name}, {<<"email">>, Email}, {<<"hash">>, Hash}]),
    
    % Return the JSON string
    JsonString.

    

-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
-endif.