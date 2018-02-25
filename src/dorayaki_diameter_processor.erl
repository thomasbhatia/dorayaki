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
%% @doc Dorayaki diameter_processor public API
%% @end
%%%-------------------------------------------------------------------

-module(dorayaki_diameter_processor).
-copyright('Copyright (c) 2016 Thomas Bhatia').
-author('thomas.bhatia@eo.io').

-define(APPLICATION, dorayaki).

-export([process_packet/2]).

-include("dorayaki.hrl").

-define(AVP_PADDING, [3, 263, 264, 283, 293, 296]).
-define(AVP_Type_Unsigned32, [268]).bit
-define(AVP_SUPPORT, [268, 456]).

%%====================================================================
%% Records
%%====================================================================

-record(diameter_message,
        {version::byte(),                     %%  8-bit unsigned
         length :: non_neg_integer(),         %% 24-bit unsigned
         is_request :: 0 | 1,                 %% boolean() R flag
         is_proxiable :: 0 | 1,               %% boolean() P flag
         is_error :: 0 | 1,                   %% boolean() E flag
         is_retransmitted :: 0 | 1,           %% 24-bit unsigned
         cmd_code :: non_neg_integer(),       %% 32-bit unsigned
         application_id :: non_neg_integer(), %% 32-bit unsigned
         hop_by_hop_id :: non_neg_integer(),  %% 32-bit unsigned
         end_to_end_id :: non_neg_integer(),  %% boolean() T flag
         raw_data :: [any()],
         avps  = [] :: [any()]
        }).

-record(diameter_avp_new,
        {code :: non_neg_integer(),                         %% 32-bit unsigned
         v :: 0 | 1,                          
         m :: 0 | 1,
         p :: 0 | 1,
         length :: non_neg_integer(),
         value :: bitstring() | [[any()],...] | integer(),  %% decoded term() decoded | undefined
         padding :: 0 | 1 | 2 | 3 | 8 | 16 | 24 | 32,       %% Padding = length - 8
         raw_data :: bitstring(),
         grouped = [] :: [any()]
        }).

-record(state, {client, server}).

%%====================================================================
%% Type Spec
%%====================================================================
-type avp_headers() :: list({version, any()}        |
                             {length, any()}        |
                             {request, any()}       |
                             {proxiable, any()}     |
                             {error, any()}         |
                             {retransmitted, any()} |
                             {reserved, any()}      |
                             {commandcode, any()}   |
                             {appId, any()}         |
                             {endToEndId, any()}    |
                             {hopByHopId, any()}).
-type avp_header_bin() :: binary(). % <<_:160>>
-type diameter_message() :: #diameter_message{}.
-type diameter_avp_new() :: #diameter_avp_new{}.
%%====================================================================
%% TEST API
%%====================================================================
-ifdef(TEST).
-export([do_process_packet/1]).
-export([get_padding/2]).
-define(SearchHeader, [{commandcode, 272}]).
-define(SearchAVPs, [{?Result_Code, 2001}, {?Multiple_Services_Credit_Control, [{?Result_Code, 4012}]}]).
-define(ReplaceAVP, [{?Result_Code, 4012}]).
-else.
%% Here get SearchHeader and SearchAVPs from config_loader.
-define(SearchHeader, dorayaki_config_loader:get_env(search_header)).
-define(SearchAVPs, dorayaki_config_loader:get_env(search_avps)).
%% Here get ReplaceAVP from config_loader.
-define(ReplaceAVP, dorayaki_config_loader:get_env(replace_avp)).
-endif.

%%====================================================================
%% @doc Dorayaki diameter_processor public API
%% @end
%%====================================================================

-define(Replace, [?ReplaceAVP]).

% 0. Preprocess
-spec process_packet(false | 
                     binary() | 
                     {false, _, _, _} | 
                     {true, _, _, _}  | 
                     {true, boolean(), _, _, _}, 
                     #state{client::port(), server::port()}) -> {noreply, #state{client::port(), server::port()}}.
process_packet(Data, State) ->
    lager:log(debug, console, "CHECK 5."),
    case do_process_packet(Data) of
        [] ->
            lager:log(debug, console, "CHECK 6."),
            OutData = Data;

        Bin ->
            lager:log(debug, console, "CHECK 7."),
            OutData = Bin
    end,

    gen_tcp:send(State#state.client, OutData),
    inet:setopts(State#state.server, [{active, once}]),
    {noreply, State}.


%%====================================================================
%% @private Internal functions
%%====================================================================

% 1. Check Diameter Headers
-spec do_process_packet(false           | 
                        binary()        | 
                        {false, _, _, _}| 
                        {true, _, _, _} | 
                        {true, boolean(), _, _, _}) -> binary() | [].
do_process_packet(<<Bin/binary>>) ->
    % Check if Headers match search
    lager:log(debug, console, "do_process_packet 1. ~w", [<<Bin/binary>>]),
    do_process_packet(is_header_match(<<Bin/binary>>));

% 2a. Header match
do_process_packet({true, AVPBins, HeaderList, DiaMessage}) ->
    lager:log(debug, console, "Headers match, next check if AVPs match."),
    % Check if any AVPs match search
    Acc = [],
    do_process_packet(is_AVP_match(true, AVPBins, Acc, HeaderList, DiaMessage));

% 2b. Header don't match
do_process_packet({false, _, _, _}) ->
    lager:log(debug, console, "Headers do not match, abandon search."),
    [];

% 3a. Header, AVP match and accumulator
do_process_packet({true, true, Acc, HeaderList, DiaMessage}) ->
    % Edit messages
    lager:log(debug, console, "Headers match, AVPs match. We'll run 'replace' next."),
    {MessageList, HeaderList} = editor(Acc, HeaderList),
    % Pack messages
    MessageBin = packer(MessageList, HeaderList, DiaMessage),
    lager:log(debug, console, "AVPs replaced and this is final MessageBin ~w", [MessageBin]),
    MessageBin;

% 3b. AVP value don't match
do_process_packet({true, false, _Acc, _HeaderList, _DiaMessage}) ->
    lager:log(debug, console, "AVPs match but their values do not, abandon search."),
    [];

% 3c. AVP don't match
do_process_packet(false) ->
    lager:log(debug, console, "AVPs do not match, abandon search."),
    [].

%% @private
-spec get_padding(_, non_neg_integer()) -> 0 | 1 | 2 | 3 | 8 | 16 | 24 | 32.
get_padding(Code, AVP_length) ->
    case lists:member(Code, ?AVP_PADDING) of
        true  ->
            P = AVP_length rem 4,
            case P > 0 of
                true ->
                    Pad = ((P - 4) * -1) * 8;
                false ->
                    Pad = P
            end;
        false -> Pad = 0
    end,
    Pad.

%%%-------------------------------------------------------------------
%% @doc Check if AVP headers match our search criteria
%% @end
%%%-------------------------------------------------------------------
-spec is_header_match(bitstring()) -> [] | 
                                      {false, binary(), [{_, _}, ...], diameter_message()} | 
                                      {true, binary(), [{_, _}, ...], diameter_message()}.

is_header_match(<<Version:8, Length:24, R:1, P:1, E:1, T:1, Reserved:4, Command:24, AppId:32, HopByHopId:32, EndToEndId:32, Rest/binary>>) ->
    HeaderList = [{version, Version}, {length, Length}, {request, R},
        {proxiable, P}, {error, E}, {retransmitted, T}, {reserved, Reserved}, {commandcode, Command},
        {appId, AppId}, {hopByHopId, HopByHopId}, {endToEndId, EndToEndId}],

        DiaMessage = #diameter_message
        {version = Version,
         length = Length,
         is_request = R,
         is_proxiable = P,
         is_error = E,
         is_retransmitted = T,
         cmd_code = Command,
         application_id = AppId,
         hop_by_hop_id = HopByHopId,
         end_to_end_id = EndToEndId,
         raw_data = [<<Version:8, Length:24, R:1, P:1, E:1, T:1, Reserved:4, Command:24, AppId:32, HopByHopId:32, EndToEndId:32>>],
         avps = []
         }, %% boolean() T flag

    {lists:all(fun(X) -> lists:member(X, HeaderList) end, ?SearchHeader), Rest, HeaderList, DiaMessage};

is_header_match(_) ->
    lager:log(debug, console, "We don't understand the Diameter Header we recieved."),
    [].

%%%-------------------------------------------------------------------
%% @doc Check if AVP body match our search criteria
%% @end
%%%-------------------------------------------------------------------
%-spec is_AVP_match(boolean(), _, _, _, _) -> 'false' | {'true',boolean(),[{_, _}], _, _}.
is_AVP_match(true, <<Code:32, Vendor:1, Mandatory:1, Protected:1, _Reserved:5, Length:24, Rest/binary>>, Acc, HeaderList, DiaMessage) ->
    Padding = get_padding(Code, Length),
    BodyLength = ((Length * 8) - 64),
    <<Value0:BodyLength, _padding:Padding, Rest2/binary>> = <<Rest/binary>>,

    %% Optimise!
    {ok, Type} = dorayaki_avp_mapper:num_to_type(Code),
    Value = get_value(Type, <<Value0:BodyLength>>, BodyLength),
    RawData = <<Code:32, Vendor:1, Mandatory:1, Protected:1, _Reserved:5, Length:24>>, <<Value0:BodyLength, 0:Padding>>,
    AVPR1 = #diameter_avp_new{
                code = Code,
                v = Vendor,
                m = Mandatory,
                p = Protected,
                length = Length,
                value = Value,
                padding = Padding,
                raw_data = RawData},

    KSAPR = get_ksapr(Code),
    {AVPR, AVP, Search} = get_aas(KSAPR, Type, <<Value0:BodyLength>>, AVPR1, Value, Code),
    D = DiaMessage,
    N = D#diameter_message.avps,
    is_AVP_match(Search, Rest2, [AVP|Acc], HeaderList, D#diameter_message{avps = [AVPR|N]});


is_AVP_match(true, <<>>, Acc, HeaderList, DiaMessage) ->
    lager:log(debug, console, "AVP search is done, next check if all filters match."),
    lager:log(debug, console, "Accumulator ~p", [Acc]),

    AVP_filter_list_match = lists:all(fun(X) -> lists:member(X, Acc) end, ?SearchAVPs),
    lager:log(debug, console, "AVP filter list match: ~p", [AVP_filter_list_match]),

    {true, AVP_filter_list_match, Acc, HeaderList, DiaMessage};

is_AVP_match(false, _, _Acc, _HeaderList, _DiaMessage) ->
    lager:log(debug, console, "AVPs do not match"),
    false;

is_AVP_match(_, _, _, _, _) ->
    lager:log(error, console, "AVP cycle went wrong, value unknown"),
    false.

get_value(arb, Bin, _BodyLength) ->
    [binary_to_list(Bin)];
get_value(gro, Bin, _BodyLength) ->
    Bin;
get_value(_Type, Bin, BodyLength) ->
    <<Value:BodyLength>> = Bin,
    Value.

get_ksapr(Code) ->
    lists:keysearch(Code, 1, ?SearchAVPs).

get_aas({value, {Code, _}}, gro, Bin, AVPR1, RValue, Code) ->
    GAPVM = is_Grouped_AVP_match(Bin, AVPR1),
    case GAPVM of
        {true, true, GroupAcc, AVPR2} ->
            AVPR = AVPR2,
            AVP = {Code, GroupAcc},
            Search = true;
        {true, false, GroupAcc, AVPR2} ->
            AVPR = AVPR2,
            AVP = {Code, GroupAcc},
            Search = false;
        false ->
            AVPR = AVPR1,
            AVP = {Code, RValue},
            Search = false
    end,
    {AVPR, AVP, Search};

get_aas({value, {Code, Value}}, _Type, _Bin, AVPR, RValue, Code) when Value == RValue ->
    AVP = {Code, RValue},
    Search = true,
    {AVPR, AVP, Search};

get_aas({value, {Code, Value}}, _Type, _Bin, AVPR, RValue, Code) when Value /= RValue ->
    AVP = {Code, RValue},
    Search = false,
    {AVPR, AVP, Search};

get_aas(_, _Type, _Bin, AVPR, RValue, Code) ->
    AVP = {Code, RValue},
    Search = true,
    {AVPR, AVP, Search}.

%%%-------------------------------------------------------------------
%% @doc Search within Grouped AVP by unwrapping
%% @end
%%%-------------------------------------------------------------------

-spec is_Grouped_AVP_match(bitstring(), diameter_avp_new()) ->  any().
is_Grouped_AVP_match(Bin, AVPR) ->
    check_Grouped_AVP_match(true, Bin, [], AVPR).

%spec is_Grouped_AVP_match(boolean(),bitstring(),[{non_neg_integer(),[any(),...] | integer()}],#diameter_avp_new{code::non_neg_integer(),v::0 | 1,m::0 | 1,p::0 | 1,length::non_neg_integer(),value::bitstring() | [[any()],...] | integer(),padding::0 | 1 | 2 | 3 | 8 | 16 | 24 | 32,raw_data::[bitstring(),...],grouped::[{_,_,_,_,_,_,_,_,_,_}]}) -> 'false' | {'true',boolean(),[{_,_}],#diameter_avp_new{code::non_neg_integer(),v::0 | 1,m::0 | 1,p::0 | 1,length::non_neg_integer(),value::bitstring() | [any(),...] | integer(),padding::0 | 1 | 2 | 3 | 8 | 16 | 24 | 32,raw_data::[any(),...],grouped::[any()]}}.
check_Grouped_AVP_match(true, <<Code:32, Vendor:1, Mandatory:1, Protected:1, _Reserved:5, Length:24, Rest/binary>>, Acc, AVPR) ->
    lager:log(debug, console, "AVP within group, Code: ~p, Length: ~p, Record: ~p", [Code, Length, AVPR]),

    Padding = get_padding(Code, Length),
    lager:log(debug, console, "AVP within group, Padding: ~p", [Padding]),

    BodyLength = ((Length * 8) - 64),
    lager:log(debug, console, "AVP within group, BodyLength: ~p", [BodyLength]),

    AVPBin = <<Value0:BodyLength, _padding:Padding, Rest2/binary>> = <<Rest/binary>>,

    %% Optimise!
    {ok, Type} = dorayaki_avp_mapper:num_to_type(Code),
    Value = get_value(Type, <<Value0:BodyLength>>, BodyLength),

    AVP = {Code, Value},

    NewSubAVPR = #diameter_avp_new
        {code = Code,
         v = Vendor,
         m = Mandatory,
         p = Protected,
         length = Length,
         value = Value,
         padding = Padding,
         raw_data = AVPBin,
         grouped = []
         },

    lager:log(debug, console, "Grouped AVP record: ~p, value: ~p", [NewSubAVPR, AVP]),

    GroupCode = AVPR#diameter_avp_new.code,
    lager:log(debug, console, "Grouped code: ~p", [GroupCode]),

    {value, {GroupCode, SearchGroup}} = lists:keysearch(GroupCode, 1, ?SearchAVPs),
    lager:log(debug, console, "SearchGroup AVP value: ~p", [SearchGroup]),

    case lists:keysearch(Code, 1, SearchGroup) of
        % Found Code and value, set search to true
        {value, {Code, Value}} ->
            io:format(user, "Value1 ~p~n", [Value]),
            Search = true;
        % Found Code but value was not what we want, set search to false. Abandon search.
        {value, {Code, _WrongValue}} ->
            io:format(user, "Value2 ~p~n", [Value]),
            Search = false;
        % Not the code we're looking for so we'll carry on to the next avp, set search to true.
        S ->
            io:format(user, "Value3 ~p~n", [S]),
            Search = true
    end,

    N = AVPR#diameter_avp_new.grouped,
    check_Grouped_AVP_match(Search, Rest2, [AVP|Acc], AVPR#diameter_avp_new{grouped = [NewSubAVPR|N]});


check_Grouped_AVP_match(true, <<>>, Acc, AVPR) ->
    lager:log(debug, console, "Grouped AVP search is done, 
                               next check if all filters match. ~n
                               Grouped Accumulator ~p", [Acc]),
    GroupCode = AVPR#diameter_avp_new.code,
    {value, {GroupCode, SearchGroup}} = lists:keysearch(GroupCode, 1, ?SearchAVPs),

    Grouped_AVP_filter_list_match = lists:all(fun(X) -> lists:member(X, Acc) end, SearchGroup),
    lager:log(debug, console, "AVP filter list match: ~p", [Grouped_AVP_filter_list_match]),

    {true, Grouped_AVP_filter_list_match, Acc, AVPR};
check_Grouped_AVP_match(false, _, _Acc, _DiaMessage) ->
    lager:log(debug, console, "AVPs do not match"),
    false;
check_Grouped_AVP_match(_, _, _, _) ->
lager:log(error, console, "Grouped AVP cycle went wrong, value unknown"),
    false.

% End Grouped

%%%-------------------------------------------------------------------
%% @doc Edit packets payload
%% @end
%%%-------------------------------------------------------------------
-spec editor(maybe_improper_list(),_) -> {[any()],_}.
editor(Acc, HeaderList) ->
    [{Code, Value}| _Rest ] = ?ReplaceAVP,
    MessageList = lists:keyreplace(Code, 1, Acc, {Code, Value}),
    {MessageList, HeaderList}.

%%%-------------------------------------------------------------------
%% @doc Construct AVP by wrapping
%% @end
%%%-------------------------------------------------------------------
-spec packer([{integer(),_}], avp_headers() ,_) -> binary().
packer(AVPList, HeaderList, DiaMessage) ->
    lager:log(debug, console, "Packer received AVPList ~w", [AVPList]),

    AVPBins = iolist_to_binary(packAVP(AVPList, DiaMessage)),
    HeaderBin = packHeader(HeaderList),
    iolist_to_binary([HeaderBin, AVPBins]).

%%%-------------------------------------------------------------------
%% @doc Construct AVP headers
%% @end
%%%-------------------------------------------------------------------
-spec packHeader(avp_headers()) -> avp_header_bin().
packHeader([{version, Version},
            {length, Length},
            {request, Request},
            {proxiable, Proxiable},
            {error, Error},
            {retransmitted, Retransmitted},
            {reserved, Reserved},
            {commandcode, Command},
            {appId, AppId},
            {hopByHopId, HopByHopId},
            {endToEndId, EndToEndId}]) -> <<Version:8,
                                            Length:24,
                                            Request:1,
                                            Proxiable:1,
                                            Error:1,
                                            Retransmitted:1,
                                            Reserved:4,
                                            Command:24,
                                            AppId:32,
                                            HopByHopId:32,
                                            EndToEndId:32>>.

%%%-------------------------------------------------------------------
%% @doc Construct AVP body
%% @end
%%%-------------------------------------------------------------------
%-spec packAVP([{integer(),_}],_) -> [[bitstring() | tuple(),...]].
packAVP(AVPList, DiaMessage) ->
    packAVP(AVPList, [], DiaMessage).

%-spec packAVP([{integer(),_}],[[bitstring() | tuple(),...]],_) -> [[bitstring() | tuple(),...]].
packAVP([{Code, Value0}|AVPList], AVPBins, DiaMessage) ->
    lager:log(debug, console, "PackAVP recieved code ~w, value ~w", [Code, Value0]),

    Dz = DiaMessage#diameter_message.avps,
    Lz = lists:keyfind(Code, 2, Dz),
    Vz = Lz#diameter_avp_new.v,
    Mz = Lz#diameter_avp_new.m,
    Pz = Lz#diameter_avp_new.p,

    lager:log(debug, console, "AVP Vz ~w, Mz ~w, Pz ~w flag ~w", [Vz, Mz, Pz]),

    Flags = <<Vz:1,Mz:1,Pz:1,0:1,0:1,0:1,0:1,0:1>>,
    lager:log(debug, console, "AVP full flag ~w", [Flags]),

    AVPBin = case dorayaki_avp_mapper:num_to_type(Code) of
        {ok, arb} ->
            Value = list_to_binary(Value0),
            Length = get_avp_length(Value),
            Pad = get_padding(Code, Length),

            Padding = <<0:Pad>>,
            [<<Code:32>>, Flags, <<Length:24>>, Value, Padding];

        {ok, gro} ->
            Value = Lz#diameter_avp_new.value,
            Length = get_avp_length(Value),
            Pad = get_padding(Code, Length),

            Padding = <<0:Pad>>,
            [<<Code:32>>, Flags, <<Length:24>>, Value, Padding];

        {ok, Length} ->
            Value = Value0,
            [<<Code:32>>, Flags, <<Length:24>>, <<Value:32>>]
    end,
    packAVP(AVPList, [AVPBin|AVPBins], DiaMessage);
packAVP([], AVPBins, _DiaMessage) ->
    AVPBins.


get_avp_length(Value) ->
    (4+1+3+size(Value)).

