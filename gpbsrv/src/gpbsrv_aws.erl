%%%-------------------------------------------------------------------
%%% @author Shrikant Khanduri
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%% Created : 29 October 2018
%%%-------------------------------------------------------------------
-module(gpbsrv_aws).

%% API
-export([db_init/1, 
	 db_init/3, 
	 db_init/5,
	 db_get_item/2, 
	 db_insert_item/3,
	 kms_init/1,
	 kms_encrypt/2,
	 kms_decrypt/1
	]).

-define(KEY, <<"key">> ).
-define(KEYTYPE, s ).
-define(VALUE, <<"value">> ).
-define(DEFAULT_RD, 5 ).
-define(DEFAULT_WR, 5 ).

%%--------------------------------------------------------------------

db_init(Tablename) ->
    db_init(Tablename, ?KEY, ?KEYTYPE, ?DEFAULT_RD, ?DEFAULT_WR). 

db_init(Tablename, Key, Key_type) ->
    db_init(Tablename, Key, Key_type, ?DEFAULT_RD, ?DEFAULT_WR).

db_init(Tablename, Key, Key_type,  Read, Write) ->
   case lists:member(Tablename, element(2, erlcloud_ddb:list_tables())) of 
	true ->
	    ok;
	false ->
            Ddb_key_type = get_ddb_key_type(Key_type),
            erlcloud_ddb:create_table(Tablename, {Key, Ddb_key_type}, Read, Write)
    end,
   spawn(fun() -> db_ops(Tablename) end).

get_ddb_key_type(string) -> s;
get_ddb_key_type(integer) -> n;
get_ddb_key_type(int) -> n;
get_ddb_key_type(bool) -> b;
get_ddb_key_type(boolean) -> b.

db_ops(Tablename) ->
    receive
	{Pid, insert_item, Attr_list} ->
	    Pid ! erlcloud_ddb:put_item(Tablename, Attr_list);
	{Pid, get_item, Key, Key_type} ->
            Pid ! erlcloud_ddb:get_item(Tablename,{Key_type, Key})
    end,
    db_ops(Tablename).

db_insert_item(Ref, Key, Values) ->
    Ref ! {self(), insert_item, [Key | Values]},
    receive
        {ok, _}    -> ok;
	{error, _} -> error
    end.

db_get_item(Ref, {Key, Key_type}) ->
    Ref ! {self(), get_item, Key, get_ddb_key_type(Key_type)},
    receive
	{ok, Response} -> Response;
        _ -> error
    end.


-ifdef('KMS_ENABLED').

kms_init(Aliasname) ->
    {ok, Aliases} = erlcloud_kms:list_aliases(),
    Kms_keys     = [ {proplists:get_value(<<"AliasName">>,Alias),
                      proplists:get_value(<<"TargetKeyId">>, Alias)} 
                      || Alias <- proplists:get_value(<<"Aliases">>,Aliases) ],
    case proplists:get_value(Aliasname, Kms_keys) of
	undefined -> 
            {ok, Value} = erlcloud_kms:create_key(),
	    Kms_key = proplists:get_value(<<"KeyId">>,
				proplists:get_value(<<"KeyMetadata">>, Value)),
	    erlcloud_kms:update_alias(Aliasname, Kms_key);
	Kms_key -> Kms_key
    end.

kms_encrypt(Kms_key, Value) ->
    {ok, Resp} =  erlcloud_kms:encrypt(Kms_key, Value),
    proplists:get_value(<<"CiphertextBlob">>, Resp).

kms_decrypt(Value) ->
    {ok, Resp} = erlcloud_kms:decrypt(Value),
     proplists:get_value(<<"Plaintext">>, Resp).

-else.
kms_init(Aliasname) -> Aliasname.

kms_encrypt(_Kms_key, Value) -> Value.

kms_decrypt(Value) -> Value.

-endif.
