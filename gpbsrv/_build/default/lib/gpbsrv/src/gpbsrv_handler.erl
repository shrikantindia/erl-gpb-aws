%%%-------------------------------------------------------------------
%%% @author Shrikant Khanduri
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%% Created : 29 October 2018
%%%-------------------------------------------------------------------
-module(gpbsrv_handler).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("gpb_def.hrl").
-define(SERVER, ?MODULE).
-define(DEFAULT_TABLENAME, "gpbsrv").
-define(DB_KEYNAME, <<"key">> ).
-define(DB_KEY_TYPE, string).
-define(DB_VALUENAME, <<"value">> ).
-define(DEFAULT_ALIASNAME, "alias/challenge").

-record(state, {dbref, tablename, kms_key}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Tablename = 
	case application:get_env(gpbsrv, db_tablename) of 
          undefined   -> list_to_binary(?DEFAULT_TABLENAME);
	  {ok, Value} -> list_to_binary(Value)
	end,
    Dbref = gpbsrv_aws:db_init(Tablename, ?DB_KEYNAME, ?DB_KEY_TYPE),

    Aliasname = case application:get_env(gpbsrv, kms_alias) of
          undefined   -> list_to_binary(?DEFAULT_ALIASNAME);
          {ok, Alias} -> list_to_binary(Alias)
        end,

    Kms_key = gpbsrv_aws:kms_init(Aliasname),
    {ok, #state{dbref = Dbref, tablename = Tablename, kms_key = Kms_key}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Buffer, _From, State) ->
    Msg   = gpb_def:decode_msg(Buffer, req_envelope, []),
    ok    = gpb_def:verify_msg(Msg, req_envelope),
    #req_envelope{type = Type} = Msg,
    Reply = process_message(State#state.dbref,State#state.kms_key, Type, Msg),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
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
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_response(set_response_t, Response) ->
    #req_envelope{type = set_response_t, set_resp = 
		  #set_response{error = Response}};

create_response(get_response_t, {ok, Response}) ->
    Key   = proplists:get_value(?DB_KEYNAME, Response),
    Value = proplists:get_value(?DB_VALUENAME, Response),
    Dvalue = gpbsrv_aws:kms_decrypt(Value),
    Data  = #data{ key = Key, value = Dvalue },
    #req_envelope{type = get_response_t, get_resp = 
		  #get_response{error = ok, req = Data}};

create_response(get_response_t, Response) ->
    #req_envelope{type = get_response_t, 
		  get_resp = #get_response{error = Response}}.

process_message(Dbref, Kms_key, set_request_t, #req_envelope{set_req = Set_req}) ->
    #set_request{req = #data{key = Key, value = Value}} = Set_req,
    Evalue = gpbsrv_aws:kms_encrypt(Kms_key, list_to_binary(Value)),
    Response = 
        case gpbsrv_aws:db_insert_item(Dbref, 
			   {?DB_KEYNAME, Key}, [{?DB_VALUENAME,Evalue}]) of 
	    {error, _} -> internal;
	    ok         -> ok
	end,
    Msg = create_response(set_response_t, Response),
    gpb_def:encode_msg(Msg, req_envelope, []);

process_message(Dbref, _Kms_key, get_request_t, #req_envelope{get_req = Get_req}) ->
    #get_request{key = Key} = Get_req,
    Response = 
        case gpbsrv_aws:db_get_item(Dbref, {Key, string}) of
	    []     -> not_found;
	    error  -> internal;
	    Result -> {ok, Result}
	end,
    Msg = create_response(get_response_t, Response),
    gpb_def:encode_msg(Msg, req_envelope, []).


