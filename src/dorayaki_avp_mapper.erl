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

%%%-------------------------------------------------------------------
%% @doc Dorayaki avp_mapper public API
%% @end
%%%-------------------------------------------------------------------

-module(dorayaki_avp_mapper).
-copyright('Copyright (c) 2016 Thomas Bhatia').
-author('thomas.bhatia@eo.io').

-include("dorayaki.hrl").

-export([num_to_type/1]).

-spec num_to_type(_) -> 'error' | {'ok', _}.
num_to_type(Num) ->
    AVP_TYPE = [
        {1, ?UTF8String},
        {3, ?Grouped},
        {25, ?OctetString},
        {27, ?Unsigned32},
        {33, ?OctetString},
        {44, ?OctetString},
        {50, ?UTF8String},
        {55, ?Time},
        {85, ?Unsigned32},
        {257, ?Address},
        {258, ?Unsigned32},
        {259, ?Unsigned32},
        {260, ?Grouped},
        {261, ?Enumerated},
        {262, ?Unsigned32},
        {263, ?UTF8String},
        {264, ?DiamIdent},
        {265, ?Unsigned32},
        {266, ?Unsigned32},
        {267, ?Unsigned32},
        {268, ?Unsigned32},
        {269, ?UTF8String},
        {270, ?Unsigned32},
        {271, ?Enumerated},
        {272, ?Unsigned32},
        {273, ?Enumerated},
        {274, ?Enumerated},
        {276, ?Unsigned32},
        {277, ?Enumerated},
        {278, ?Unsigned32},
        {279, ?Grouped},
        {280, ?DiamIdent},
        {281, ?UTF8String},
        {282, ?DiamIdent},
        {283, ?DiamIdent},
        {284, ?Grouped},
        {285, ?Enumerated},
        {287, ?Unsigned64},
        {291, ?Unsigned32},
        {292, ?DiamURI},
        {293, ?DiamIdent},
        {294, ?DiamIdent},
        {295, ?Enumerated},
        {296, ?DiamIdent},
        {297, ?Grouped},
        {298, ?Unsigned32},
        {299, ?Unsigned32},
        {456, ?Grouped},
        {480, ?Enumerated},
        {483, ?Enumerated},
        {485, ?Unsigned32}],

        D = dict:from_list(AVP_TYPE),
        V = dict:find(Num, D),
    V.
