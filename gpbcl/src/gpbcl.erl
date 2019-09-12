%%%-------------------------------------------------------------------
%%% @author Shrikant Khanduri
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%% Created : 29 October 2018
%%%-------------------------------------------------------------------
-module(gpbcl).

%% API
-export([set_request/2, get_request/1]).

set_request(Key, Value) ->
	gpbcl_srv:set_request(Key, Value).

get_request(Key) ->
	 gpbcl_srv:get_request(Key). 

