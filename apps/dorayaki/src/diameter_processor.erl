%%%-------------------------------------------------------------------
%% @doc dorayaki diameter_processor public API
%% @end
%%%-------------------------------------------------------------------

-module(diameter_processor).

% -define(TEST, test).

-define(APPLICATION, dorayaki).

-export([process_packet/2]).

-include("lib.hrl").

-define(AVP_PADDING, [3, 263, 264, 283, 293, 296]).
-define(AVP_Type_Unsigned32, [268]).bit
-define(AVP_SUPPORT, [268, 456]).

%%====================================================================
%% Records
%%====================================================================
-record(diameter_message,
        {version,            %%  8-bit unsigned
         length,             %% 24-bit unsigned
         is_request,         %% boolean() R flag
         is_proxiable,       %% boolean() P flag
         is_error,           %% boolean() E flag
         is_retransmitted,
         cmd_code,           %% 24-bit unsigned
         application_id,     %% 32-bit unsigned
         hop_by_hop_id,      %% 32-bit unsigned
         end_to_end_id,      %% 32-bit unsigned
         raw_data,
         avps = []
         }). %% boolean() T flag


-record(diameter_avp_new,
        {code,      %% 32-bit unsigned
         v,           
         m,
         p,
         length,
         value,     %% decoded term() decoded | undefined
         padding,    %% Padding = length - 8
         raw_data,
         grouped = []
         }).  


-record(state, {client, server}).

%%====================================================================
%% TEST API
%%====================================================================
-ifdef(TEST).

-define(Message, <<1,0,1,56,0,0,1,16,0,0,0,4,100,111,77,129,100,
        111,77,129,0,0,1,7,64,0,0,35,107,121,108,101,45,
        114,48,53,45,103,103,115,110,48,51,54,59,48,59,
        56,55,50,57,53,48,53,56,0,0,0,1,2,64,0,0,12,0,0,
        0,4,0,0,1,8,64,0,0,11,79,67,71,0,0,0,1,40,64,0,
        0,22,111,99,103,46,104,117,97,119,101,105,46,99,
        111,109,0,0,0,0,0,55,64,0,0,12,216,49,110,93,0,
        0,1,22,64,0,0,12,255,255,255,255,0,0,1,12,64,0,
        0,12,0,0,7,209,0,0,1,187,64,0,0,40,0,0,1,194,64,
        0,0,12,0,0,0,0,0,0,1,188,64,0,0,19,51,52,54,49,
        49,51,57,57,57,48,51,0,0,0,1,159,64,0,0,12,0,0,
        0,0,0,0,1,160,64,0,0,12,0,0,0,1,0,0,1,200,0,0,0,
        108,0,0,1,12,64,0,0,12,0,0,7,209,0,0,1,175,64,0,
        0,36,0,0,1,164,64,0,0,12,0,152,150,127,0,0,1,
        165,0,0,0,16,0,0,0,0,0,16,0,0,0,0,1,176,0,0,0,
        12,0,0,0,3,0,0,1,183,0,0,0,12,255,255,255,255,0,
        0,3,101,192,0,0,16,0,0,40,175,0,3,32,0,0,0,1,
        192,64,0,0,12,0,0,7,8>>).

-define(SearchHeader, [{commandcode, 271}]).
-define(SearchAVPs, [{?Result_Code, 2001}, {?Multiple_Services_Credit_Control, [{?Result_Code, 4012}]}]).
-define(ReplaceAVP, [{?Result_Code, 4012}]).

-else.

%% Here get SearchHeader and SearchAVPs from config_loader.
-define(SearchHeader, config_loader:get_env(search_header)).
-define(SearchAVPs, config_loader:get_env(search_avps)).

%% Here get ReplaceAVP from config_loader.
-define(ReplaceAVP, config_loader:get_env(replace_avp)).

-endif.

%%====================================================================
%% API
%%====================================================================

-define(Replace, [?ReplaceAVP]).

% 0. Preprocess
process_packet(Data, State) ->
    lager:log(debug, "console", "CHECK 5."),
    case process_packet(Data) of 
        [] -> 
            lager:log(debug, "console", "CHECK 6."),
            OutData = Data;

        Bin ->
            lager:log(debug, "console", "CHECK 7."),
            OutData = Bin
    end,

    gen_tcp:send(State#state.client, OutData),
    inet:setopts(State#state.server, [{active, once}]),
    {noreply, State}.


% 1. Check Diameter Headers 
process_packet(<<Bin/binary>>) ->
    % io:format("Bin is ~p~n", [<<Bin/binary>>]),
    % Check if Headers match search
    lager:log(debug, "console", "process_packet 1. ~w", [<<Bin/binary>>]),
    process_packet(is_header_match(<<Bin/binary>>));

% 2a. Header match
process_packet({true, AVPBins, HeaderList, DiaMessage}) -> 
    lager:log(debug, "console", "Headers match, next check if AVPs match."),
    % Check if any AVPs match search
    Acc = [],
    process_packet(is_AVP_match(true, AVPBins, Acc, HeaderList, DiaMessage));

% 2b. Header don't match
process_packet({false, _, _, _}) -> 
    lager:log(debug, "console", "Headers do not match, abandon search."),
    [];

% 3a. Header, AVP match and accumulator
process_packet({true, true, Acc, HeaderList, DiaMessage})->
    % Edit messages
    lager:log(debug, "console", "Headers match, AVPs match. We'll run 'replace' next."),
    {MessageList, HeaderList} = editor(Acc, HeaderList),
    % Pack messages
    MessageBin = packer(MessageList, HeaderList, DiaMessage),
    lager:log(debug, "console", "AVPs replaced and this is final MessageBin ~w", [MessageBin]),
    MessageBin;

% 3b. AVP value don't match
process_packet({true, false, _Acc, _HeaderList, _DiaMessage}) ->
    lager:log(debug, "console", "AVPs match but their values do not, abandon search."),
    [];

% 3c. AVP don't match
process_packet(false) ->
    lager:log(debug, "console", "AVPs do not match, abandon search."),
    [].


%%====================================================================
%% Internal functions
%%====================================================================
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

%%%%%%%%
% Check header match
%%%%%%%%
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
    lager:log(debug, "console", "We don't understand the Diameter Header we recieved."),
    [].

%%%%%%%%
% Check AVPs match
%%%%%%%%
is_AVP_match(true, <<Code:32, Vendor:1, Mandatory:1, Protected:1, _Reserved:5, Length:24, Rest/binary>>, Acc, HeaderList, DiaMessage) ->
    lager:log(debug, "console", "Start matching AVP"),
    lager:log(debug, "console", "AVP Code: ~w", [Code]),
    lager:log(debug, "console", "AVP Value Type: ~w", [dorayaki_avp_mapper:num_to_type(Code)]),
    lager:log(debug, "console", "AVP Length: ~w", [Length]),
    lager:log(debug, "console", "Diameter message received: ~p", [DiaMessage]),

    Padding = get_padding(Code, Length),
    lager:log(debug, "console", "AVP Padding: ~w", [Padding]),

    BodyLength = ((Length * 8) - 64),
    lager:log(debug, "console", "AVP BodyLength: ~w", [BodyLength]),

    AVPBin = <<_Value:BodyLength, _padding:Padding, Rest2/binary>> = <<Rest/binary>>,

    %% Optimise!
    Type = dorayaki_avp_mapper:num_to_type(Code),
    case Type of 
        {ok, arb} ->
            Value = [binary_to_list(<<_Value:BodyLength>>)];
        {ok, gro} ->
            Value = <<_Value:BodyLength>>;
        {ok, _Type} ->
            Value = _Value
    end,

    AVPR1 = #diameter_avp_new
    {code = Code,    
     v = Vendor,  
     m = Mandatory,  
     p = Protected,  
     length = Length,
     value = Value,
     padding = Padding,
     raw_data = [<<Code:32, Vendor:1, Mandatory:1, Protected:1, _Reserved:5, Length:24>>, <<_Value:BodyLength, 0:Padding>>]
     },  

    % io:format("GROUP key search~p~n", [lists:keysearch(Code, 1, ?SearchAVPs)]),
    KSAPR = lists:keysearch(Code, 1, ?SearchAVPs),
    lager:log(debug, "console", "Key search within AVPs result: ~p", [KSAPR]),
    case KSAPR of
        % Found Code and value, but AVP is a group AVP, must unwrap to find the truth
        {value, {Code, _}} when Type == {ok, gro} ->
            GAPVM = is_Grouped_AVP_match(<<_Value:BodyLength>>, AVPR1),
            lager:log(debug, "console", "Check if AVP is grouped and match: ~p", [GAPVM]),
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
                    AVP = {Code, Value},
                    Search = false
            end;                    
        % Found Code and value, set search to true
        {value, {Code, Value}} ->
            AVPR = AVPR1,
            AVP = {Code, Value},
            Search = true;
        % Found Code but value was not what we want, set search to false. Abandon search.
        {value, {Code, _WrongValue}} ->
            AVPR = AVPR1,
            AVP = {Code, Value},
            Search = false;
        % Not the code we're looking for so we'll carry on to the next avp, set search to true.
        _ ->
            AVPR = AVPR1,
            AVP = {Code, Value},
            Search = true
    end,

    lager:log(debug, "console", "AVP record: ~p", [AVPR]),
    lager:log(debug, "console", "AVP value: ~p", [AVP]),

    D = DiaMessage,
    N = D#diameter_message.avps,

    is_AVP_match(Search, Rest2, [AVP|Acc], HeaderList, D#diameter_message{avps = [AVPR|N]});


is_AVP_match(true, <<>>, Acc, HeaderList, DiaMessage) ->
    lager:log(debug, "console", "AVP search is done, next check if all filters match."),
    lager:log(debug, "console", "Accumulator ~p", [Acc]),

    AVP_filter_list_match = lists:all(fun(X) -> lists:member(X, Acc) end, ?SearchAVPs),
    lager:log(debug, "console", "AVP filter list match: ~p", [AVP_filter_list_match]),

    {true, AVP_filter_list_match, Acc, HeaderList, DiaMessage};

is_AVP_match(false, _, _Acc, _HeaderList, _DiaMessage) ->
    lager:log(debug, "console", "AVPs do not match"),
    false;

is_AVP_match(_, _, _, _, _) ->
    lager:log(error, "console", "AVP cycle went wrong, value unknown"),
    false.

%%% Grouped AVP search
is_Grouped_AVP_match(Bin, AVPR) ->
    Acc = [],
    AVPRecordList = [],
    is_Grouped_AVP_match(true, Bin, Acc, AVPR).

is_Grouped_AVP_match(true, <<Code:32, Vendor:1, Mandatory:1, Protected:1, _Reserved:5, Length:24, Rest/binary>>, Acc, AVPR) ->
    lager:log(debug, "console", "Start matching within Group AVP"),
    lager:log(debug, "console", "AVP within group, Code: ~p", [Code]),
    lager:log(debug, "console", "AVP within group, Length: ~p", [Length]),
    lager:log(debug, "console", "AVP record received: ~p", [AVPR]),

    Padding = get_padding(Code, Length),
    lager:log(debug, "console", "AVP within group, Padding: ~p", [Padding]),

    BodyLength = ((Length * 8) - 64),
    lager:log(debug, "console", "AVP within group, BodyLength: ~p", [BodyLength]),

    AVPBin = <<_Value:BodyLength, _padding:Padding, Rest2/binary>> = <<Rest/binary>>,

    %% Optimise!
    Type = dorayaki_avp_mapper:num_to_type(Code),
    case Type of 
        {ok, arb} ->
            Value = [binary_to_list(<<_Value:BodyLength>>)];
        {ok, _Type} ->
            Value = _Value
    end,

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
    
    lager:log(debug, "console", "Grouped AVP record: ~p", [NewSubAVPR]),
    lager:log(debug, "console", "Grouped AVP value: ~p", [AVP]),

    GroupCode = AVPR#diameter_avp_new.code,
    lager:log(debug, "console", "Grouped code: ~p", [GroupCode]),

    {value, {GroupCode, SearchGroup}} = lists:keysearch(GroupCode, 1, ?SearchAVPs),
    lager:log(debug, "console", "SearchGroup AVP value: ~p", [SearchGroup]),

    case lists:keysearch(Code, 1, SearchGroup) of
        % Found Code and value, set search to true
        {value, {Code, Value}} ->
            Search = true;
        % Found Code but value was not what we want, set search to false. Abandon search.
        {value, {Code, _WrongValue}} ->
            Search = false;
        % Not the code we're looking for so we'll carry on to the next avp, set search to true.
        _ ->
            Search = true
    end,

    N = AVPR#diameter_avp_new.grouped,

    is_Grouped_AVP_match(Search, Rest2, [AVP|Acc], AVPR#diameter_avp_new{grouped = [NewSubAVPR|N]});


is_Grouped_AVP_match(true, <<>>, Acc, AVPR) ->
    lager:log(debug, "console", "Grouped AVP search is done, next check if all filters match."),
    lager:log(debug, "console", "Grouped Accumulator ~p", [Acc]),

    GroupCode = AVPR#diameter_avp_new.code,
    {value, {GroupCode, SearchGroup}} = lists:keysearch(GroupCode, 1, ?SearchAVPs),
    Grouped_AVP_filter_list_match = lists:all(fun(X) -> lists:member(X, Acc) end, SearchGroup),

    lager:log(debug, "console", "AVP filter list match: ~p", [Grouped_AVP_filter_list_match]),

    {true, Grouped_AVP_filter_list_match, Acc, AVPR};

is_Grouped_AVP_match(false, _, _Acc, _DiaMessage) ->
    lager:log(debug, "console", "AVPs do not match"),
    false;

is_Grouped_AVP_match(_, _, _, _) ->
lager:log(error, "console", "Grouped AVP cycle went wrong, value unknown"),
    false.

% End Grouped

%%%%%%%%%
% EDITOR
%%%%%%%%%
editor(Acc, HeaderList) ->
    lager:log(debug, "console", "Editor received accumulator ~w", [Acc]),
    [{Code, Value}| Rest ] = ?ReplaceAVP,
    lager:log(debug, "console", "Value to be replaced: ~p", [{Code, Value}]),

    MessageList = lists:keyreplace(Code, 1, Acc, {Code, Value}),
    lager:log(debug, "console", "Edited Message is now: ~p", [MessageList]),
    {MessageList, HeaderList}.

%%%%%%%%%
% PACKER
%%%%%%%%%
packer(AVPList, HeaderList, DiaMessage) ->
    lager:log(debug, "console", "Packer received AVPList ~w", [AVPList]),

    AVPBins = iolist_to_binary(packAVP(AVPList, DiaMessage)),
    HeaderBin = packHeader(HeaderList),
    iolist_to_binary([HeaderBin, AVPBins]).

packHeader([{version, Version}, {length, Length}, {request, R}, 
        {proxiable, P}, {error, E}, {retransmitted, T}, {reserved, Reserved}, {commandcode, Command}, 
        {appId, AppId}, {hopByHopId, HopByHopId}, {endToEndId, EndToEndId}]) ->
    <<Version:8, Length:24, R:1, P:1, E:1, T:1, Reserved:4, Command:24, AppId:32, HopByHopId:32, EndToEndId:32>>.
   

packAVP(AVPList, DiaMessage) ->
    packAVP(AVPList, [], DiaMessage).

packAVP([{Code, _Value}|AVPList], AVPBins, DiaMessage) ->
    lager:log(debug, "console", "PackAVP recieved code ~w", [Code]),
    lager:log(debug, "console", "PackAVP recieved value ~w", [_Value]),

    Dz = DiaMessage#diameter_message.avps,
    Lz = lists:keyfind(Code, 2, Dz),

    Vz = Lz#diameter_avp_new.v,
    Mz = Lz#diameter_avp_new.m,
    Pz = Lz#diameter_avp_new.p,

    lager:log(debug, "console", "AVP V flag ~w", [Vz]),
    lager:log(debug, "console", "AVP V flag ~w", [Mz]),
    lager:log(debug, "console", "AVP V flag ~w", [Pz]),

    Flags = <<Vz:1,Mz:1,Pz:1,0:1,0:1,0:1,0:1,0:1>>,
    lager:log(debug, "console", "AVP full flag ~w", [Flags]),

    case dorayaki_avp_mapper:num_to_type(Code) of 
        {ok, arb} ->            
            Value = list_to_binary(_Value),
            lager:log(debug, "console", "AVP value: ~w", [Value]),

            Length = (4+1+3+size(Value)),
            lager:log(debug, "console", "AVP length: ~w", [Length]),

            Pad = get_padding(Code, Length),
            lager:log(debug, "console", "AVP Pad ~w", [Pad]),

            Padding = <<0:Pad>>,
            
            AVPBin = [<<Code:32>>, Flags, <<Length:24>>, Value, Padding];

        {ok, gro} ->
            Value = Lz#diameter_avp_new.value,
            lager:log(debug, "console", "AVP value: ~w", [Value]),

            Length = (4+1+3+size(Value)),
            lager:log(debug, "console", "AVP length: ~w", [Length]),

            Pad = get_padding(Code, Length),
            lager:log(debug, "console", "AVP Pad ~w", [Pad]),

            Padding = <<0:Pad>>,
            
            AVPBin = [<<Code:32>>, Flags, <<Length:24>>, Value, Padding];

        {ok, Length} ->
            Value = _Value,
            lager:log(debug, "console", "AVP value: ~w", [Value]),
            lager:log(debug, "console", "AVP length ~w", [Length]),

            AVPBin = [<<Code:32>>, Flags, <<Length:24>>, <<Value:32>>]
    end,
    lager:log(debug, "console", "AVP Bin is now wrapped: ~w", [AVPBin]),
    packAVP(AVPList, [AVPBin|AVPBins], DiaMessage);
    
packAVP([], AVPBins, DiaMessage) ->
    AVPBins.





