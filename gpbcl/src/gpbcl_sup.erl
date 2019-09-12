%%%-------------------------------------------------------------------
%% @doc gpbcl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(gpbcl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8765).
%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, Ip} = application:get_env(gpbcl, server_ip),
    Port = case application:get_env(gpbcl, server_port) of
        undefined  -> ?DEFAULT_PORT;
            {ok,Value} -> Value
           end,

    {ok, {{one_for_all, 0, 1}, 
	  [{gpbcl, {gpbcl_srv, start_link, [Ip, Port]},
              permanent, 2000, worker, [gpbcl]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
