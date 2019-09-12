gpbsrv
=====

A google proto buf / TCP based OTP application

Build
-----

    $ rebar3 compile

Application
===========

The GPB server exchange messages in the Google proto buf format over TCP. The request & response format are defined in a proto file.
As a first step, For encoding & decoding of the messages, the gpb_def files are created from the erl GPB application by using proto file as a base.
The server & client (if an erlang client) use these erl files, to encode or decode the messages.

The GPB Server
==============
The gpbsrv supervisor supervises two workers, 
- TCP/IP Engine
- Message handler
Message handler manages the internal execution while communicator interacts with the external clients.
At startup, Handler initiates the necessary DynamoDB & KMS action, while TCP Engine starts the listener and spawns multiple sub server to accept connection from multiple clients. 
The erlcloud interface is used to communicate with the AWS cloud applications.

The server accepts below requests from the client 
	- set_request
	- get_request
The server provide below response to the client
	- set_response
	- get_response

Server Initial configuration
============================
Provide environment to the gpbsrv/src/gpbsrv.app.src
	The server listening port (OPTIONAL, default port for application is 8765)
		{server_port, 9876}
		  
	The DynamoDB table name (OPTIONAL, default table name is gpbsrv)	  		
		{db_tablename, "challenge"}
		  
	The AWS key information (Can be provided through Linux Environment variable as well)	  
		{aws_access_key_id, <Your AWS Access Key>}
		{aws_secret_access_key, <Your AWS Secret Access Key>}
		{aws_region, <Your region>}
		
	If Security token is supported, the below environment also needs to be set
		{aws_security_token, <Your AWS Security Token>}

	If encryption through KMS is supported, the below environment is also required and alias name must start with 'alias/', example "alias/challenge"
		{kms_aliasname, ""}

	You can provide your amazon credentials in OS environmental variables

		export AWS_ACCESS_KEY_ID=<Your AWS Access Key>
		export AWS_SECRET_ACCESS_KEY=<Your AWS Secret Access Key>
		export AWS_SESSION_TOKEN=<Your AWS Security Token>
		export AWS_DEFAULT_REGION=<Your region>
	
Starting Server
=============== 
From gpbsrv folder, execute the below rebar command.
	$ rebar3 shell
In case of success, the erlang shell should start without any error & with the information of listening port.


NOTE : Encryption (KMS) is currently disabled.



