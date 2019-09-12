gpbsrv
=====

A google proto buf / TCP based OTP application

Build
-----

    $ rebar3 compile

Application
===========

The GPB server exchange messages in the Google proto buf format over TCP. The request & response format are defined in a proto file.
As a first step, For encoding & decoding of the messages, the gpb_def files are created from the erl GPB application by using the proto file.
The server & client (if an erlang client) use these erl files, to encode or decode the messages.

The GPB Client
==============
The gpbcl supervisor supervises the client worker. Client initiates the connection to the server at start-up. Set request insert the entry to the DynamoDB, and Get request fetches the value corresponding to the key.

The client exposes the below API for client use 
	> gpbcl:set_request(<KEY>, <VALUE>).
	> gpbcl:get_request:get_request(<KEY>).
	> gpbcl:stop().

Example 
	> gpbcl:set_request("91", "INDIA").
	> gpbcl:get_request:get_request("91").

Client Initial configuration
============================
Adding the path of rebar3 to the path
	$export PATH=$PATH:<PATH>/rebar3

Provide below environment to the gpbcl/src/gpbcl.app.src

	The server ip in erlang tuple format
		{server_ip, {127,0,0,1}}
	The server port (if undefined, default port 8765 will be used)
		{port, 9876}

Starting Client
===============
From gpbsrv folder, execute the below rebar command.
	$ rebar3 shell
In case of success, the erlang shell should start without any error. Make sure that server is started first, otherwise the shell will prompt error, if so start the server and start the client application by application:start(gpbcl).

NOTE : Encryption (KMS) is currently disabled.



