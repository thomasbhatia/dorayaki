%%%-------------------------------------------------------------------
%% @hidden
%%%-------------------------------------------------------------------

-module(dorayaki_config_loader_tests).
-copyright('Copyright (c) 2016 Thomas Bhatia').
-author('thomas.bhatia@eo.io').

-ifdef(TEST).
  
-include_lib("eunit/include/eunit.hrl").

-endif.


load_config_test() ->
	lager:start(),
	dorayaki_config_loader:load_config(),

	{ok, Client_port}	= application:get_env(dorayaki, client_port), 
	{ok, Host_ip}		= application:get_env(dorayaki, host_ip),
	{ok, Host_port}		= application:get_env(dorayaki, host_port),
	{ok, Search_header}	= application:get_env(dorayaki, search_header), 
	{ok, Search_AVPs}	= application:get_env(dorayaki, search_avps), 
	{ok, Replace_AVP}	= application:get_env(dorayaki, replace_avp), 

    ?assertEqual(3869, Client_port),
    ?assertEqual("192.168.56.102", Host_ip),
    ?assertEqual(3868, Host_port),
	?assertEqual([{commandcode, 272}], Search_header),
	?assertEqual([{456, [{268, 4012}]}], Search_AVPs),
	?assertEqual([{268, 4012}], Replace_AVP).	


