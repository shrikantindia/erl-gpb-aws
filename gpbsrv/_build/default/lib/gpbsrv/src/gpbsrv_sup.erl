%%%-------------------------------------------------------------------
%%% @author Shrikant Khanduri
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%% Created : October 29 2018
%%%-------------------------------------------------------------------
-module(gpbsrv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8765).
%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    start_erlcloud(),
    
    Port = case application:get_env(gpbsrv, server_port) of 
               undefined  -> ?DEFAULT_PORT;
	       {ok,Value} -> Value
	   end,

    Decoder = {gpbsrv_handler, {gpbsrv_handler, start_link, []},
              Restart, Shutdown, Type, [gpbsrv_handler]},
    Interface = {gpbsrv_tcp, {gpbsrv_tcp, start_link, [Port]},
              Restart, Shutdown, Type, [gpbsrv_tcp]},

 %%   Listener = {gpbsrv_tcp, {gpbsrv_tcp, start_link, []},
 %%             Restart, Shutdown, Type, [gpbsrv_tcp]},

    %%{ok, {SupFlags, [Decoder, Listener]}}.
    {ok, {SupFlags, [Decoder, Interface]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

set_erlcloud_env(Env) ->
    case application:get_env(gpbsrv, Env) of
        {ok, Value} -> application:set_env(erlcloud, Env, Value);
	    _ -> undefined
    end.

start_erlcloud() ->
    {ok,_} = application:ensure_all_started(erlcloud),
    set_erlcloud_env(aws_access_key_id),
    set_erlcloud_env(aws_secret_access_key),
    set_erlcloud_env(aws_region),
    set_erlcloud_env(aws_security_token),
    ok.

