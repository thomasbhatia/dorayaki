%%%-------------------------------------------------------------------
%% @doc dorayaki diameter_processor public API
%% @end
%%%-------------------------------------------------------------------

-module(diameter_processor).

% -define(TEST, test).

-define(APPLICATION, dorayaki).

-export([process_packet/1]).

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
-define(SearchAVPs, [{?Auth_Session_State, 1}, {?Multiple_Services_Credit_Control, [{?Result_Code, 4012}]}]).
% -define(SearchAVPs, [{?Result_Code, 2001}, {?Multiple_Services_Credit_Control, [{?Result_Code, 4012}]}]).
% -define(SearchAVPs, [{?Result_Code, 2001}, {?Auth_Session_State, 1}, {?Host_IP_Address, 2}]).
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

% 1. Check Diameter Headers 
process_packet(<<Bin/binary>>) ->
    % io:format("Bin is ~p~n", [<<Bin/binary>>]),
    % Check if Headers match search
    process_packet(is_header_match(<<Bin/binary>>));

% 2a. Header match
process_packet({true, AVPBins, HeaderList, DiaMessage}) -> 
    io:format("Headers match~n"),
    % Check if any AVPs match search
    Acc = [],
    process_packet(is_AVP_match(true, AVPBins, Acc, HeaderList, DiaMessage));

% 2b. Header don't match
process_packet({false, _, _, _}) -> 
    io:format("header no match~n"),
    [];

% 3a. Header, AVP match and accumulator
process_packet({true, true, Acc, HeaderList, DiaMessage})->
    % Edit messages
    io:format("next do replace here~n"),
    {MessageList, HeaderList} = editor(Acc, HeaderList),
    % Pack messages
    MessageBin = packer(MessageList, HeaderList, DiaMessage),
    io:format("MessageBin~p~n", [MessageBin]),
    MessageBin;

% 3b. AVP value don't match
process_packet({true, false, _Acc, _HeaderList, _DiaMessage}) ->
    io:format("nnno match~n"),
    [];

% 3c. AVP don't match
process_packet(false) ->
    io:format("no match, bye~n"),
    [].


%%====================================================================
%% Internal functions
%%====================================================================
get_padding(Code, AVP_length) ->
    case lists:member(Code, ?AVP_PADDING) of
        true  -> 
            P = AVP_length rem 4,
            % io:format("P is  ~w~n", [P]),
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
    io:format("No understand! ~n"),
    [].

%%%%%%%%
% Check AVPs match
%%%%%%%%
is_AVP_match(true, <<Code:32, Vendor:1, Mandatory:1, Protected:1, _Reserved:5, Length:24, Rest/binary>>, Acc, HeaderList, DiaMessage) ->
    % io:format("~n"),
    io:format("Length ~p~n", [Length]),

    Padding = get_padding(Code, Length),
    io:format("Padding is ~p~n", [Padding]),

    BodyLength = ((Length * 8) - 64),
    io:format("BodyLength is ~p~n", [BodyLength]),

    AVPBin = <<_Value:BodyLength, _padding:Padding, Rest2/binary>> = <<Rest/binary>>,

    io:format("IT Code ~p~n", [Code]),
    io:format("IT ~p~n", [dorayaki_avp_mapper:num_to_type(Code)]),

    %% Optimise!
    Type = dorayaki_avp_mapper:num_to_type(Code),
    case Type of 
        {ok, arb} ->
            Value = [binary_to_list(<<_Value:BodyLength>>)];
        {ok, gro} ->
            Value = <<_Value:BodyLength>>;
            % is_Grouped_AVP_match(<<_Value:BodyLength>>),
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

    io:format("GROUP key search~p~n", [lists:keysearch(Code, 1, ?SearchAVPs)]),
    io:format("WHat is TYpe:~p~n", [Type]),
    case lists:keysearch(Code, 1, ?SearchAVPs) of
        % Found Code and value, but AVP is a group AVP, must unwrap to find the truth
        {value, {Code, _}} when Type == {ok, gro} ->
        % {value,{456,[{268,2001}]}}}
        % {456,<<0,0,1,12,64,0,0,12,0,0,7,209>>}
            io:format("Jackpot!!!~n"),
            io:format("is_Grouped_AVP_match ~p~n", [is_Grouped_AVP_match(<<_Value:BodyLength>>, AVPR1)]),
            case is_Grouped_AVP_match(<<_Value:BodyLength>>, AVPR1) of
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

    io:format("got AVP with value ~p~n", [AVP]),

    %% Optimise

    D = DiaMessage,
    N = D#diameter_message.avps,
    io:format("The d is ~p~n", [D]),
    io:format("Super search is ~p~n", [Search]),
    is_AVP_match(Search, Rest2, [AVP|Acc], HeaderList, D#diameter_message{avps = [AVPR|N]});


is_AVP_match(true, <<>>, Acc, HeaderList, DiaMessage) ->
    io:format("All done, check if all filters match~n"),
    io:format("Acc is now ~p~n", [Acc]),
    io:format("SearchAVPs is ~p~n", [?SearchAVPs]),
    Stat = lists:all(fun(X) -> lists:member(X, Acc) end, ?SearchAVPs),
    io:format("Stat is ~p~n", [Stat]),
    {true, Stat, Acc, HeaderList, DiaMessage};

is_AVP_match(false, _, _Acc, _HeaderList, _DiaMessage) ->
    io:format("got false, abandon search~n"),
    false;

is_AVP_match(_, _, _, _, _) ->
    io:format("got weird something~n"),
    false.

%%% Grouped AVP search
is_Grouped_AVP_match(Bin, AVPR) ->
    Acc = [],
    AVPRecordList = [],
    is_Grouped_AVP_match(true, Bin, Acc, AVPR).

is_Grouped_AVP_match(true, <<Code:32, Vendor:1, Mandatory:1, Protected:1, _Reserved:5, Length:24, Rest/binary>>, Acc, AVPR) ->
    % io:format("~n"),
    io:format("Length ~p~n", [Length]),

    Padding = get_padding(Code, Length),
    io:format("Padding is ~p~n", [Padding]),

    BodyLength = ((Length * 8) - 64),
    io:format("BodyLength is ~p~n", [BodyLength]),

    AVPBin = <<_Value:BodyLength, _padding:Padding, Rest2/binary>> = <<Rest/binary>>,

    io:format("IT Code ~p~n", [Code]),
    io:format("IT ~p~n", [dorayaki_avp_mapper:num_to_type(Code)]),

    %% Optimise!
    case dorayaki_avp_mapper:num_to_type(Code) of 
        {ok, arb} ->
            Value = [binary_to_list(<<_Value:BodyLength>>)];
        {ok, _Type} ->
            Value = _Value
    end,

    AVP = {Code, Value},
    io:format("got AVP with value ~p~n", [AVP]),

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
    
    io:format("Code: ~p~n", [Code]),
    GroupCode = AVPR#diameter_avp_new.code,
    io:format("GroupCode: ~p~n", [GroupCode]),
    io:format("Search: ~p~n", [lists:keysearch(GroupCode, 1, ?SearchAVPs)]),

    {value, {GroupCode, SearchGroup}} = lists:keysearch(GroupCode, 1, ?SearchAVPs),
    io:format("SearchGroup: ~p~n", [SearchGroup]),

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

    %% Optimise!

    N = AVPR#diameter_avp_new.grouped,
    
    io:format("The d is ~p~n", [N]),
    io:format("The Search is ~p~n", [Search]),

    is_Grouped_AVP_match(Search, Rest2, [AVP|Acc], AVPR#diameter_avp_new{grouped = [NewSubAVPR|N]});


is_Grouped_AVP_match(true, <<>>, Acc, AVPR) ->
    io:format("All done, check if all filters match~n"),
    io:format("Acc is now ~p~n", [Acc]),
    io:format("SearchAVPs is ~p~n", [?SearchAVPs]),
    GroupCode = AVPR#diameter_avp_new.code,
    io:format("GroupCode: ~p~n", [GroupCode]),
    io:format("Search: ~p~n", [lists:keysearch(GroupCode, 1, ?SearchAVPs)]),

    {value, {GroupCode, SearchGroup}} = lists:keysearch(GroupCode, 1, ?SearchAVPs),

    Stat = lists:all(fun(X) -> lists:member(X, Acc) end, SearchGroup),
    io:format("Sub Stat is ~p~n", [Stat]),
    io:format("AVPR is ~p~n", [AVPR]),
    {true, Stat, Acc, AVPR};

is_Grouped_AVP_match(false, _, _Acc, _DiaMessage) ->
    io:format("got false, abandon search~n"),
    false;

is_Grouped_AVP_match(_, _, _, _) ->
    io:format("got weird something~n"),
    false.

% End Grouped

%%%%%%%%%
% EDITOR
%%%%%%%%%
editor(Acc, HeaderList) ->
    io:format("True, we'll replace AVP values here~p~n", [Acc]),
    [{Code, Value}| Rest ] = ?ReplaceAVP,
    io:format("H is ~p~n", [{Code, Value}]),
    MessageList = lists:keyreplace(Code, 1, Acc, {Code, Value}),
    io:format("HeaderList is now ~p~n", [HeaderList]),
    io:format("Message is ~p~n", [MessageList]),
    {MessageList, HeaderList}.

%%%%%%%%%
% PACKER
%%%%%%%%%
packer(AVPList, HeaderList, DiaMessage) ->
    io:format("HeaderList is ~p~n", [HeaderList]),

    AVPBins = iolist_to_binary(packAVP(AVPList, DiaMessage)),
    % io:format("Packed AVPBin is ~p~n", [AVPBin]),

    HeaderBin = packHeader(HeaderList),
    % io:format("HeaderBin is ~p~n", [HeaderBin]),
    io:format("Actual size of Header is ~p~n", [size(HeaderBin)]),

    io:format("Actual size of AVPBinis ~p~n", [size(AVPBins)]),

    iolist_to_binary([HeaderBin, AVPBins]).

packHeader([{version, Version}, {length, Length}, {request, R}, 
        {proxiable, P}, {error, E}, {retransmitted, T}, {reserved, Reserved}, {commandcode, Command}, 
        {appId, AppId}, {hopByHopId, HopByHopId}, {endToEndId, EndToEndId}]) ->
    <<Version:8, Length:24, R:1, P:1, E:1, T:1, Reserved:4, Command:24, AppId:32, HopByHopId:32, EndToEndId:32>>.
   

packAVP(AVPList, DiaMessage) ->
    packAVP(AVPList, [], DiaMessage).

packAVP([{Code, _Value}|AVPList], AVPBins, DiaMessage) ->
    io:format("Code is ~p~n", [Code]),
    io:format("Value is ~p~n", [_Value]),
    % io:format("AVPBins is ~p~n", [AVPBins]),

    Dz = DiaMessage#diameter_message.avps,
    Lz = lists:keyfind(Code, 2, Dz),
    io:format("L is now now now ~p~n", [Lz]),

    Vz = Lz#diameter_avp_new.v,
    Mz = Lz#diameter_avp_new.m,
    Pz = Lz#diameter_avp_new.p,

    io:format("V is now ~p~n", [Vz]),
    io:format("M is now ~p~n", [Mz]),
    io:format("P is now ~p~n", [Pz]),

    Flags = <<Vz:1,Mz:1,Pz:1,0:1,0:1,0:1,0:1,0:1>>,
    io:format("Flags is ~p~n", [Flags]),

    case dorayaki_avp_mapper:num_to_type(Code) of 
        {ok, arb} ->            
            Value = list_to_binary(_Value),
            io:format("Value is ~p~n", [Value]),

            Length = (4+1+3+size(Value)),
            io:format("Length is ~p~n", [Length]),

            Pad = get_padding(Code, Length),
            io:format("Pad is ~p~n", [Pad]),

            Padding = <<0:Pad>>,
            
            AVPBin = [<<Code:32>>, Flags, <<Length:24>>, Value, Padding];

        {ok, gro} ->
            Value = Lz#diameter_avp_new.value,
            io:format("Value is ~p~n", [Value]),

            Length = (4+1+3+size(Value)),
            io:format("Length is ~p~n", [Length]),

            Pad = get_padding(Code, Length),
            io:format("Pad is ~p~n", [Pad]),

            Padding = <<0:Pad>>,
            
            AVPBin = [<<Code:32>>, Flags, <<Length:24>>, Value, Padding];

        {ok, Size} ->
            Value = _Value,
            io:format("Last Size is ~p~n", [Size]),

            Length = Size,
            io:format("Length is ~p~n", [Length]),

            AVPBin = [<<Code:32>>, Flags, <<Length:24>>, <<Value:32>>]
    end,
    io:format("Pile is ~p~n", [AVPBin]),
    io:format("~n"),
    packAVP(AVPList, [AVPBin|AVPBins], DiaMessage);
    
packAVP([], AVPBins, DiaMessage) ->
    AVPBins.


%     decode_avps(Rest2, SearchAVPs, [AVPBin|OutBinList], MatchedAVPList);


% % append_attr(Attr, State) ->
% % State#decoder_state{attrs = [Attr | State#decoder_state.attrs]}.





