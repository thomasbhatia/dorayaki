%%%-------------------------------------------------------------------
%% @hidden
%%%-------------------------------------------------------------------

-module(dorayaki_diameter_processor_tests).
-copyright('Copyright (c) 2016 Thomas Bhatia').
-author('thomas.bhatia@eo.io').

-ifdef(TEST).
 
-include_lib("eunit/include/eunit.hrl").

-endif.


match_process_packet_test() ->
	Bin = <<1,0,0,176,0,0,1,16,0,0,0,4,0,0,7,11,0,0,10,243,0,0,1,7,64,0,0,24,46,
         59,49,48,57,54,50,57,56,51,57,49,59,56,48,51,0,0,1,4,64,0,0,32,0,0,1,
         2,64,0,0,12,0,0,1,2,0,0,1,3,64,0,0,12,0,0,0,4,0,0,1,8,64,0,0,30,105,
         100,101,102,105,120,46,103,114,101,110,111,98,108,101,46,104,112,46,
         99,111,109,0,0,0,0,1,40,64,0,0,23,103,114,101,110,111,98,108,101,46,
         104,112,46,99,111,109,0,0,0,1,21,64,0,0,12,0,0,0,1,0,0,1,12,64,0,0,12,
         0,0,7,209,0,0,1,200,64,0,0,20,0,0,1,12,64,0,0,12,0,0,15,172>>,
	Result = dorayaki_diameter_processor:process_packet(Bin),

	?assertEqual(<<1,0,0,176,0,0,1,16,0,0,0,4,0,0,7,11,0,0,10,243,0,0,1,
		7,64,0,0,24,46,59,49,48,57,54,50,57,56,51,57,49,59,56,48,51,0,0,1,4,64,0,
		0,32,0,0,1,2,64,0,0,12,0,0,1,2,0,0,1,3,64,0,0,12,0,0,0,4,0,0,1,8,64,0,0,30,
		105,100,101,102,105,120,46,103,114,101,110,111,98,108,101,46,104,112,46,99,
		111,109,0,0,0,0,1,40,64,0,0,23,103,114,101,110,111,98,108,101,46,104,112,46,
		99,111,109,0,0,0,1,21,64,0,0,12,0,0,0,1,0,0,1,12,64,0,0,12,0,0,15,172,0,0,1,
		200,64,0,0,20,0,0,1,12,64,0,0,12,0,0,15,172>>, Result).


no_match_process_packet_test() ->
	Bin = <<1,0,0,176,0,0,1,16,0,0,0,4,0,0,5,74,0,0,9,50,0,0,1,7,64,0,0,24,46,59,
		49,48,57,54,50,57,56,51,57,49,59,51,53,52,0,0,1,4,64,0,0,32,0,0,1,2,64,0,
		0,12,0,0,1,2,0,0,1,3,64,0,0,12,0,0,0,4,0,0,1,8,64,0,0,30,105,100,101,102,
		105,120,46,103,114,101,110,111,98,108,101,46,104,112,46,99,111,109,0,0,0,
		0,1,40,64,0,0,23,103,114,101,110,111,98,108,101,46,104,112,46,99,111,109,
		0,0,0,1,21,64,0,0,12,0,0,0,1,0,0,1,12,64,0,0,12,0,0,15,172,0,0,1,200,64,0,
		0,20,0,0,1,12,64,0,0,12,0,0,15,172>>,
	Result = dorayaki_diameter_processor:process_packet(Bin),

	?assertEqual([], Result).


match_get_padding_test() ->
	AVP_Codes = [3, 263, 264, 283, 293, 296],
	AVP_length = 21,
	match_get_padding(AVP_Codes, AVP_length).

match_get_padding([AVP_Code|AVP_Codes], AVP_length) ->
	Result = dorayaki_diameter_processor:get_padding(AVP_Code, AVP_length),
	?assertEqual(24, Result),
	match_get_padding(AVP_Codes, AVP_length);

match_get_padding([], _AVP_length) ->
	{ok, done}.


no_match_get_padding_test() ->
	AVP_Codes = [1, 164, 266, 282, 291, 300],
	AVP_length = 21,
	no_match_get_padding(AVP_Codes, AVP_length).

no_match_get_padding([AVP_Code|AVP_Codes], AVP_length) ->
	Result = dorayaki_diameter_processor:get_padding(AVP_Code, AVP_length),
	?assertEqual(0, Result),
	no_match_get_padding(AVP_Codes, AVP_length);

no_match_get_padding([], _AVP_length) ->
	{ok, done}.
