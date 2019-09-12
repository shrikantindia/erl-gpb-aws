%%%-------------------------------------------------------------------
%%% @author Shrikant Khanduri
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%% Created : 29 October 2018
%%%-------------------------------------------------------------------
-module(gpbcl).
-behaviour(gen_server).

%% API
-export([start_link/2, set_request/2, get_request/1, stop/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("gpb_def.hrl").
-define(SERVER, ?MODULE).
-define(OPTS_CONNECT, [binary, {active, false}, {packet, 0},{keepalive,true}]).
-define(CONN_TIMEOUT, 3).
-record(state,{sock, ip, port}).

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
start_link(Ip, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Ip, Port], []).

set_request(Key, Value) ->
    case {verify_input(Key), verify_input(Value)} of 
	{ invalid_input, _ } -> invalid_input;
	{ _, invalid_input } -> invalid_input;
	{ Bin_key, Bin_value } -> 
		    gen_server:call(?MODULE, 
	                {req_envelope, set_request, Bin_key, Bin_value})
    end.

get_request(Key) ->
    case verify_input(Key) of
        invalid_input -> invalid_input;
        Bin_key -> 
            gen_server:call(?MODULE, {req_envelope, get_request, Bin_key})
    end.

stop() -> gen_server:stop(?MODULE).

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
init([Ip, Port]) ->
    try
        process_flag(trap_exit, true),
        Sock = connect(Ip, Port),
        {ok, #state{sock = Sock, ip = Ip,  port = Port}}
    catch
        _Class:Error ->
            {stop, Error}
    end.

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
handle_call(Msg, _From, State) ->
    Bin_req  = create_request(Msg),
    case send_request(State#state.sock, Bin_req) of
        {error, _} ->
	    gen_tcp:close(State#state.sock),
	    New_sock = connect(State#state.ip, State#state.port),
	    case send_request(New_sock, Bin_req) of
		{error, Error} ->
		    gen_tcp:close(New_sock),
		    {stop, closed, {error, Error}, State};
		Response ->
	            Reply = decode_message(Response),
		    {reply, Reply, #state{sock = New_sock}}
	    end;
        Response ->
            Reply = decode_message(Response),
            {reply, Reply, State}
    end.

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
handle_info(tcp_closed, State) ->
    gen_tcp:close(State#state.sock),
    {stop, closed, State};

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
terminate(_Reason, State) ->
    gen_tcp:close(State#state.sock).

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

verify_input(Input) when is_bitstring(Input) -> Input;
verify_input(Input) when is_list(Input) -> list_to_binary(Input);
verify_input(_Input) -> invalid_input.

connect(Ip, Port) ->
    {ok, Sock } = gen_tcp:connect(Ip, Port, ?OPTS_CONNECT),
    Sock.

recv_resp(Sock) ->
    case gen_tcp:recv(Sock, 0, 1000 * ?CONN_TIMEOUT) of
	{ok, Data} -> Data;
	Error      -> erlang:throw(Error)
    end.

send_request(Sock, Message) ->
    ok = gen_tcp:send(Sock, Message),
    recv_resp (Sock).

create_request({req_envelope, set_request, Key, Value}) ->
    Data     = #data{key = Key, value = Value},
    Set_req  = #set_request{req = Data},
    Msg      = #req_envelope{type = set_request_t, set_req = Set_req},
    gpb_def:encode_msg(Msg, req_envelope, []);

create_request({req_envelope, get_request, Key}) ->
    Get_req  = #get_request{key = Key},
    Msg      = #req_envelope{type = get_request_t, get_req = Get_req},
    gpb_def:encode_msg(Msg, req_envelope, []).
    
process_message(#req_envelope{type = set_response_t, set_resp = 
					      #set_response{error = Error}}) ->
    Error;

process_message(#req_envelope{type = get_response_t, get_resp = 
		    #get_response{error = ok, req = #data{value = Value}}}) ->
    Value;

process_message(#req_envelope{type = get_response_t, get_resp =
					      #get_response{error = Error}}) ->
    {error, Error}.

decode_message(Buffer) ->
    Msg      = gpb_def:decode_msg(Buffer, req_envelope, []),
    ok       = gpb_def:verify_msg(Msg, req_envelope),
    process_message(Msg).

