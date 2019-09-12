%%%-------------------------------------------------------------------
%%% @author Shrikant Khanduri
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%% Created : 29 October 2018
%%%-------------------------------------------------------------------
-module(gpbsrv_tcp).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(MESSAGE_HANDLER, gpbsrv_handler).
-define(SERVER_MAX, 20).

-record(state, {lsock, pids = []}).

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
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

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
init([Port]) ->
    process_flag(trap_exit, true),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, 
					 {reuseaddr, true}]),
    io:format("Server started, listening at port ~p~n",[Port]),
    [spawn(fun() -> server(LSock) end) || _ <- lists:seq(1,?SERVER_MAX)],
    {ok, #state{lsock = LSock}}.

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
handle_call(stop, _From, State) ->
    [gen_tcp:close(Pid) || Pid <- State#state.pids],
    {stop, normal, ok, State}.

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
handle_cast({started, Pid}, #state{pids = Pids} = State) ->
    {noreply, State#state{pids = [ Pid | Pids]}}.

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
terminate(_Reason, State) ->
    gen_tcp:close(State#state.lsock),
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

get_request(Sock) ->
    case gen_tcp:recv(Sock, 0) of 
        {error, Error} -> 
		{error, Error};
	{ok, Msg} -> 
		Response = gen_server:call(?MESSAGE_HANDLER, Msg),
                gen_tcp:send(Sock, Response),
                get_request(Sock)
    end.

server(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            gen_server:cast(?MODULE,{started, Sock}),
            get_request(Sock),
            server(LSock);
        _Otherwise ->
            ok
    end.

