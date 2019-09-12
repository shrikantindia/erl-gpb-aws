%%%-------------------------------------------------------------------
%% @doc gpbcl public API
%% @end
%%%-------------------------------------------------------------------

-module(gpbcl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    gpbcl_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
