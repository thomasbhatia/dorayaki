%%%-------------------------------------------------------------------
%% @hidden
%%%-------------------------------------------------------------------

-module(dorayaki_avp_mapper_tests).
-copyright('Copyright (c) 2016 Thomas Bhatia').
-author('thomas.bhatia@eo.io').

-ifdef(TEST).
  
-include_lib("eunit/include/eunit.hrl").

-endif.


known_num_to_type_test() ->
	Avp_type_arb = dorayaki_avp_mapper:num_to_type(1),
	Avp_type_int_32 = dorayaki_avp_mapper:num_to_type(27),
	Avp_type_int_64 = dorayaki_avp_mapper:num_to_type(287),
	Avp_type_gro = dorayaki_avp_mapper:num_to_type(284),

	?assertEqual({ok, arb}, Avp_type_arb),
	?assertEqual({ok, 12}, Avp_type_int_32),
	?assertEqual({ok, 16}, Avp_type_int_64),
	?assertEqual({ok, gro}, Avp_type_gro).


unknown_num_to_type_test() ->
	Avp_type_unknown = dorayaki_avp_mapper:num_to_type(999),

	?assertEqual(error, Avp_type_unknown).