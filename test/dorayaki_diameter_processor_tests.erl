%% Copyright (c) 2018, Thomas Bhatia <thomas.bhatia@eo.io>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are
%% met:
%%
%% * Redistributions of source code must retain the above copyright
%%   notice, this list of conditions and the following disclaimer.
%%
%% * Redistributions in binary form must reproduce the above copyright
%%   notice, this list of conditions and the following disclaimer in the
%%   documentation and/or other materials provided with the distribution.
%%
%% * The names of its contributors may not be used to endorse or promote
%%   products derived from this software without specific prior written
%%   permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(dorayaki_diameter_processor_tests).

-include_lib("eunit/include/eunit.hrl").


dorayaki_diameter_processor_test_() ->
    [{"Match packets when procssing", fun match_process_packet_test/0},
     {"No match packets when procossing", fun no_match_process_packet_test/0},
     {"Match get padding test", fun match_get_padding_test/0},
     {"No match get padding test", fun no_match_get_padding_test/0}
    ].

match_process_packet_test() ->
    Bin = <<1,0,0,176,0,0,1,16,0,0,0,4,0,0,7,11,0,0,10,243,0,0,1,7,64,0,0,24,46,
         59,49,48,57,54,50,57,56,51,57,49,59,56,48,51,0,0,1,4,64,0,0,32,0,0,1,
         2,64,0,0,12,0,0,1,2,0,0,1,3,64,0,0,12,0,0,0,4,0,0,1,8,64,0,0,30,105,
         100,101,102,105,120,46,103,114,101,110,111,98,108,101,46,104,112,46,
         99,111,109,0,0,0,0,1,40,64,0,0,23,103,114,101,110,111,98,108,101,46,
         104,112,46,99,111,109,0,0,0,1,21,64,0,0,12,0,0,0,1,0,0,1,12,64,0,0,12,
         0,0,7,209,0,0,1,200,64,0,0,20,0,0,1,12,64,0,0,12,0,0,15,172>>,
    Result = dorayaki_diameter_processor:do_process_packet(Bin),

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
    Result = dorayaki_diameter_processor:do_process_packet(Bin),

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
