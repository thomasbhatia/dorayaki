%%%-------------------------------------------------------------------
%% @doc dorayaki diameter_processor public API
%% @end
%%%-------------------------------------------------------------------

-module(diameter_processor).

-define(TEST, false).

-define(APPLICATION, dorayaki).

-export([process_packet/1]).

-include("diameter.hrl").
-include("lib.hrl").


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
-define(SearchAVPs, [{?Result_Code, 2001}]).
% -define(SearchAVPs, [{?Result_Code, 2001}, {?Multiple_Services_Credit_Control, [{?Result_Code, 4012}]}]).
% -define(SearchAVPs, [{?Result_Code, 2001}, {?Auth_Session_State, 1}, {?Host_IP_Address, 2}]).
-define(ReplaceAVP, [{?Result_Code, 4012}]).

-else.

%% Here get SearchHeader and SearchAVPs from config_loader.
-define(SearchHeader, []).
-define(SearchAVPs, []).

%% Here get ReplaceAVP from config_loader.
-define(ReplaceAVP, []).

-endif.

%%====================================================================
%% API
%%====================================================================

-define(Replace, [?ReplaceAVP]).

% 1. Check Diameter Headers 
process_packet(<<Bin/binary>>) ->
    io:format("Bin is ~p~n", [<<Bin/binary>>]),
    % Check if Headers match search
    process_packet(is_header_match(<<Bin/binary>>));

% 2a. Header match
process_packet({true, AVPBins, HeaderList}) -> 
    % Check if any AVPs match search
    process_packet(is_AVP_match(true, AVPBins, [], HeaderList));

% 2b. Header don't match
process_packet({false, _, _}) -> 
    io:format("header no match"),
    [];

% 3a. Header, AVP match and accumulator
process_packet({true, true, Acc, HeaderList})->
    % Edit messages
    io:format("next do replace here~n"),
    % {MessageList, HeaderList} = editor(Acc, HeaderList),
    % Pack messages
    % MessageBin = packer(MessageList, HeaderList),
    % Test packing
    MessageBin = packer(Acc, HeaderList),
    io:format("MessageBin~p~n", [MessageBin]),
    MessageBin;

% 3b. AVP value don't match
process_packet({true, false, _Acc, _HeaderList}) ->
    io:format("nnno match~n"),
    [];

% 3c. AVP don't match
process_packet(false) ->
    io:format("no match, bye"),
    [].


%%====================================================================
%% Internal functions
%%====================================================================
%%%%%%%%
% Check header match
%%%%%%%%
is_header_match(<<Version:8, Length:24, R:1, P:1, E:1, T:1, Reserved:4, Command:24, AppId:32, HopByHopId:32, EndToEndId:32, Rest/binary>>) ->
    HeaderList = [{version, Version}, {length, Length}, {request, R}, 
        {proxiable, P}, {error, E}, {retransmitted, T}, {reserved, Reserved}, {commandcode, Command}, 
        {appId, AppId}, {hopByHopId, HopByHopId}, {endToEndId, EndToEndId}],
    {lists:all(fun(X) -> lists:member(X, HeaderList) end, ?SearchHeader), Rest, HeaderList};

is_header_match(_) ->
    io:format("No understand! ~n"),
    [].

%%%%%%%%
% Check AVPs match
%%%%%%%%
is_AVP_match(true, <<Code:32, Flags:8, Length:24, Rest/binary>>, Acc, HeaderList) ->
    % io:format("~n"),
    io:format("Length ~p~n", [Length]),

    Padding = diameter_codec:get_padding(Code, Length),
    io:format("Padding is ~p~n", [Padding]),

    BodyLength = ((Length * 8) - 64),
    io:format("BodyLength is ~p~n", [BodyLength]),

    <<_Value:BodyLength, _padding:Padding, Rest2/binary>> = <<Rest/binary>>,

    io:format("IT Code ~p~n", [Code]),
    io:format("IT ~p~n", [dorayaki_avp_mapper:num_to_type(Code)]),

    case dorayaki_avp_mapper:num_to_type(Code) of 
        {ok, arb} ->
            Value = [binary_to_list(<<_Value:BodyLength>>)];
        {ok, gro} ->
            [Value] = [binary_to_list(<<_Value:BodyLength>>)];
        {ok, _Type} ->
            Value = _Value
    end,

    AVP = {Code, Value},
    io:format("got AVP with value ~p~n", [AVP]),

    case lists:keysearch(Code, 1, ?SearchAVPs) of
        % Found Code and value, set search to true
        {value, {Code, Value}} ->
            Search = true;
        % Found Code but value was not what we want, set search to false. Abandon search.
        {value, {Code, _WrongValue}} ->
            Search = false;
        % Found unknown code, carry on to next avp, set search to true.
        _ ->
            Search = true
    end,

    is_AVP_match(Search, Rest2, [AVP|Acc], HeaderList);


is_AVP_match(true, <<>>, Acc, HeaderList) ->
    io:format("All done, check if all filters match~n"),
    io:format("Acc is now ~p~n", [Acc]),
    io:format("SearchAVPs is ~p~n", [?SearchAVPs]),
    Stat = lists:all(fun(X) -> lists:member(X, Acc) end, ?SearchAVPs),
    io:format("Stat is ~p~n", [Stat]),
    {true, Stat, Acc, HeaderList};

is_AVP_match(false, _, _Acc, _HeaderList) ->
    io:format("got false, abandon search~n"),
    false;

is_AVP_match(_, _, _, _) ->
    io:format("got weird something~n"),
    false.


% 456 MSCC
% decode_packet(<<456:32, Flags:8, Length:24, Result_Code/bitstring, Rest/binary>>, Acc, HeaderList) ->
%     decode_packet(Rest, [{456, Result_Code}|Acc], HeaderList).

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
packer(AVPList, HeaderList) ->
    io:format("HeaderList is ~p~n", [HeaderList]),

    AVPBin = iolist_to_binary(packAVP(AVPList)),
    % io:format("Packed AVPBin is ~p~n", [AVPBin]),

    {value,{length, Length}} = lists:keysearch(length, 1, HeaderList),
    io:format("Length in HeaderList is ~p~n", [Length]),

    % NewHeaderList = lists:keyreplace(length, 1, HeaderList, {length, size(AVPBin)}),

    % {value,{length, NewLength}} = lists:keysearch(length, 1, NewHeaderList),
    % io:format("NEW Length in HeaderList is ~p~n", [NewLength]),

    HeaderBin = packHeader(HeaderList),
    % io:format("HeaderBin is ~p~n", [HeaderBin]),
    io:format("Actual size of Header is ~p~n", [size(HeaderBin)]),

    io:format("Actual size of AVPBinis ~p~n", [size(AVPBin)]),

    _BIN = [HeaderBin, AVPBin],
    BIN = iolist_to_binary(_BIN),
    % io:format("BIN is ~p~n", [BIN]),
    BIN.

packHeader([{version, Version}, {length, Length}, {request, R}, 
        {proxiable, P}, {error, E}, {retransmitted, T}, {reserved, Reserved}, {commandcode, Command}, 
        {appId, AppId}, {hopByHopId, HopByHopId}, {endToEndId, EndToEndId}]) ->
    <<Version:8, Length:24, R:1, P:1, E:1, T:1, Reserved:4, Command:24, AppId:32, HopByHopId:32, EndToEndId:32>>.
   

packAVP(AVPList) ->
    packAVP(AVPList, []).

packAVP([{Code, _Value}|AVPList], AVPBin) ->
    io:format("Code is ~p~n", [Code]),
    io:format("Value is ~p~n", [_Value]),
    % io:format("AVPBin is ~p~n", [AVPBin]),
    Flags = <<0:1,1:1,0:1,0:1,0:1,0:1,0:1,0:1>>,

    case dorayaki_avp_mapper:num_to_type(Code) of 
        {ok, arb} ->
            [_V] = _Value,
            
            Value = list_to_binary(_Value),
            io:format("Value is ~p~n", [Value]),
           
            ValueSize = size(Value),
            io:format("arb ValueSize is ~p~n", [ValueSize]),

            ValueSizeBit = ValueSize,
            io:format("ValueSizeBit is ~p~n", [ValueSizeBit]),

            Length = (4+1+3+ValueSizeBit),
            Pad = diameter_codec:get_padding(Code, Length),
            Padding = <<0:Pad>>,

            io:format("Length is ~p~n", [Length]),
            io:format("Code is ~p~n", [Code]),
            C = <<Code:32>>, 
            io:format("Flags is ~p~n", [Flags]),
            F = <<Flags/binary>>,   

            L = <<Length:8>>, 
            io:format("Value is ~p~n", [Value]),

            io:format("ValueSize is ~p~n", [ValueSize]),
            V = <<Value:ValueSize/bitstring>>, 
            io:format("Pad is ~p~n", [Pad]),

            % P = <<Padding:Pad/binary>>,
            Pile = [<<Code:32, 0:1,1:1,0:1,0:1,0:1,0:1,0:1,0:1, Length:24>>, Value, Padding];

        {ok, gro} ->
            Value = list_to_binary(_Value),
            io:format("Value is ~p~n", [Value]),

            ValueSize = size(Value),
            io:format("gro ValueSize is ~p~n", [ValueSize]),

            ValueSizeBit = ValueSize,
            io:format("ValueSizeBit is ~p~n", [ValueSizeBit]),

            Length = (4+1+3+ValueSizeBit),
            io:format("Length is ~p~n", [Length]),

            Pad = diameter_codec:get_padding(Code, Length),
            io:format("Pad is ~p~n", [Pad]),

            Padding = <<0:Pad>>,

            io:format("Flags is ~p~n", [Flags]),

            %D
            A = <<Code:32>>, 
            B = Flags, 
            C = <<Length:24>>, 
            % D = <<Value:ValueSizeBit>>, 
            % E = <<Padding:Pad>>,
            
            P = <<Padding:Pad/bitstring>>,
            Pile = [<<Code:32, 0:1,1:1,0:1,0:1,0:1,0:1,0:1,0:1, Length:24>>, Value, Padding];

        {ok, Size} ->
            Value = _Value,
            io:format("Last Size is ~p~n", [Size]),

            Length = Size,
            io:format("Length is ~p~n", [Length]),

            Pile = <<Code:32, 0:1,1:1,0:1,0:1,0:1,0:1,0:1,0:1, Length:24, Value:32>>
    end,
    io:format("Pile is ~p~n", [Pile]),
    io:format("~n"),
    packAVP(AVPList, [Pile|AVPBin]);
    
packAVP([], AVPBin) ->
    AVPBin.

    % case dorayaki_avp_mapper:num_to_type(Code) of 
    %     {ok, arb} ->
    %         Length = (32+8+24+ValueSize),
    %         io:format("Length is ~p~n", [Length]),
    %         Pad = diameter_codec:get_padding(Code, Length),
    %         % io:format("Padding is ~p~n", [Pad]),

    %         % io:format("Code is ~p~n", [Code]),
    %         % io:format("Flags is ~p~n", [Flags]),
    %         % io:format("Length is ~p~n", [Length]),
    %         % io:format("ValueBin is ~p~n", [ValueBin]),
    %         % io:format("ValueSize is ~p~n", [ValueSize]),
    %         % io:format("Pad is ~p~n", [Pad]),
    %         % Padding = <<0000:Pad>>,
    %         % _Code = <<Code:32>>,
    %         % _Flags = <<Flags/binary>>,
    %         % _Length = <<Length:8>>,
    %         Pile = <<Code:32, Flags:8, Length:8, ValueBin:ValueSize, Padding>>;

    %     {ok, Length} ->
    %         Pile = <<Code:32, Flags:8, Length:8, ValueBin:ValueSize>>
    % end,
    % [].


% decode_header(Bin) ->
%     decode_header(Bin, []).
% decode_header(<<Version:8, Length:24, R:1, P:1, E:1, T:1, Reserved:4, Command:24, AppId:32, HopByHopId:32, EndToEndId:32, AVPs/binary>>, []) ->
%     [{headers, [{version, Version}, {length, Length}, {request, R}, {proxiable, P}, {error, E}, {retransmitted, T}, {commandcode, Command}]}, {avps, AVPs}].




% decode_request(Sock, Bin) ->
%     io:format("sizeof Bin is ~p~n", [bit_size(Bin)]),
%     decode_request(Sock, Bin, false, <<>>).

% decode_request(Sock, <<Version:8, Length:24, Rest/binary>> = Bin, false, _) ->
%     BodyLength = ((Length * 8) - 200),
%     io:format("length~p~n", [Length]),
%     io:format("BodyLength~p~n", [BodyLength]),
%     BitLength = Length*8,
%     io:format("BitLength~p~n", [BitLength]),
%     io:format("sizeof Rest is ~p~n", [bit_size(Rest)]),
%     io:format("sizeof Bin is ~p~n", [bit_size(Bin)]),

%     <<MessageBody:BodyLength, Rest2/bitstring>> = Rest, 
%     MessageBin = <<Version:8, Length:24, MessageBody:BodyLength>>,
%     [{headers, HeadersList}, {avps, AVPs}] = diameter_codec:decode_header(MessageBin),
%     Match = lists:all(fun(X) -> lists:member(X, HeadersList) end, ?SearchHeader),
%     decode_request(Sock, Rest, Match, AVPs);

% % Messed up messages
% decode_request(Sock, <<_>> = Bin, false, []) ->
%     case gen_tcp:recv(Sock, 0, 0) of
%         {ok, BinList} ->   
%             io:format("got more data from sock~w~n", [BinList]),
%             NewBin = list_to_binary(BinList),
%             NewBin = <<Bin/binary, NewBin/binary>>;
%         {error, timeout} ->
%             io:format("got timeout~n"),
%             NewBin = Bin;
%         {error, Reason} ->
%             NewBin = Bin,
%             io:format("Errrrrooooo!! ~n"),
%             exit(Reason)
%     end,
%     decode_request(Sock, NewBin, false, []).

% decode_request(Message, true, AVPs) ->
%     {AVP, RestAVPs} = diameter_codec:decode_avps(AVPs),
%     io:format("AVP is~p~n", [AVP]),
%     case lists:member(AVP, ?SearchAVPs) of 
%         true -> 
%             decode_request(Message, true, RestAVPs);
%         false ->
%             []
%     end;

% decode_request(Message, true, <<>>) ->
%     io:format("Passed all!!!~n"),
%     [];

% decode_request(_Message, false, _AVPs) ->
%     [].




%%====================================================================
%% Internal functions
%%====================================================================


% decode_command(<<Version:8, Length:24, R:1, P:1, E:1, T:1, Reserved:4, Command:24, AppId:32, HopByHopId:32, EndToEndId:32, AVPs/binary>>, [SearchHeader, SearchAVPs], [Replace]) ->	
% 	io:format("SearchHeader is ~p~n", [SearchHeader]),
% 	HeaderList = [{version, Version}, {length, Length}, {flags, Flags}, {commandcode, Command}],
% 	io:format("HeaderList is ~p~n", [HeaderList]),
%     io:format("SearchAVPs is ~p~n", [SearchAVPs]),

%     % Check if all search terms are in Headers
% 	case lists:all(fun(X) -> lists:member(X, HeaderList) end, SearchHeader) of 
% 		true ->
% 			io:format("Search is go! ~n"),
%             OutBinList = [],
%             MatchedAVPList = [],
%             NewData = decode_avps(AVPs, SearchAVPs, OutBinList, MatchedAVPList);
            
%         false ->
%         	io:format("No search, go home! ~n"),
%             NewData = []
%     end,
%     NewData.



decode_avps(<<Code:32, Flags:8, Length:24, Rest/binary>>, SearchAVPs, OutBinList, MatchedAVPList) ->
    io:format("~n"),
    io:format("Code of AVP is ~p~n", [Code]),
	Padding = diameter_codec:get_padding(Code, Length),
    io:format("Padding is ~p~n", [Padding]),
	BodyLength = ((Length * 8) - 64) + Padding,
    io:format("BodyLength is ~p~n", [BodyLength]),
	<<BodyBin:BodyLength, Rest2/binary>> = <<Rest/binary>>,
    AVPBin = <<Code:32, Flags:8, Length:24, BodyBin:BodyLength>>,

    % case lists:keysearch(Code, 1, SearchAVPs) of
    %     {value,{Code, SearchVal}} -> 
    %         {_, Decoded_avp_value} = diameter_codec:decode_avp(AVPBin),
    %         io:format("Decoded_avp_value is: ~p~n", [Decoded_avp_value]),
    %     false ->
    %         io:format("Not the AVP you're looking for! ~n"),
    %         cheeky_avp(Rest2, SearchAVPs, [AVPBin|OutBinList], MatchedAVPList)
    % end,

    AVPSearchMatch = search_AVP(Code, SearchAVPs, AVPBin),
    io:format("AVPSearchMatch match? ~p~n", [AVPSearchMatch]),

    case AVPSearchMatch of 
        {true, AVPValue} ->
            io:format("We have match!!!!! ~w~n", [AVPSearchMatch]),
            io:format("And the value is  ~w~n", [AVPValue]),
            [{AVPSearchMatch, AVPValue}|MatchedAVPList];
        {false, _} ->
            io:format("No match ~n")
	end,
    
    decode_avps(Rest2, SearchAVPs, [AVPBin|OutBinList], MatchedAVPList);


% append_attr(Attr, State) ->
% State#decoder_state{attrs = [Attr | State#decoder_state.attrs]}.



decode_avps(<<>>, SearchAVPs, OutBinList, MatchedAVPList) ->
    io:format("Finished. OutBinList is ~w~n~n", [OutBinList]),
    io:format("MatchedAVPList is ~w~n~n", [MatchedAVPList]),
    [];

decode_avps(_, SearchAVPs, OutBinList, MatchedAVPList) ->
    io:format("Got, something weird. OutBinList is ~w~n", [OutBinList]),
    [].


search_AVP(Code, SearchAVPs, AVPBin) ->
    case lists:keysearch(Code, 1, SearchAVPs) of
        {value,{Code,SearchVal}} -> 
            % If true we decode the AVP and get the value.
            io:format("AVP found in Search! And value is: ~p~n", [SearchVal]),
            
            case diameter_codec:decode_avp(AVPBin) of
                {ok, AVPValue} ->
                    io:format("AVP_value is: ~p~n", [AVPValue]),
                    {AVPValue == SearchVal, SearchVal};
                {error, Status} ->
                    io:format("Error decoding AVP: ~p~n", [Status]),
                    {false, Status}
            end;

        false ->
            % If false we add the bin to the outbinlist
            io:format("Not the AVP you're looking for! ~n"),
            {false, false}
    end.

% %%%%%%% Sanitize 
% preprocess_data(<<Version:8, Length:24, Flags:8, Command:24, T/binary>>, {Client, Server, State})->
%     Data = <<Version:8, Length:24, Flags:8, Command:24, T/binary>>,
%     % io:format("this is data for test: ~p~n", [Data]),

%     case Command =:= 272 of 
%         true ->
%             % io:format("It's 272: ~n"),
%             process_data(Data, {Client, Server, State});
            
%         false ->
%             % io:format("Not 272 ~n"),
%             gen_tcp:send(Client, Data),
%             inet:setopts(Server, [{active, once}]),
%             {noreply, State}
%     end;

% preprocess_data(Data , {Client, Server, State})->
%     % io:format("this is BAD data: ~p~n", [Data]),
%     gen_tcp:send(Client, Data),
%     inet:setopts(Server, [{active, once}]),
%     {noreply, State}.
% %%%%%%%

% %%%%%%% process
% process_data(<<Header:160 , Body/binary>>) ->
%     % io:format("Bin: ~w~n", [Bin]),
%     Bin = <<Header:160 , Body/binary>>,

%     % io:format("=+=+=+=+=+=+=+=+=+=+=+=+= ~n~n~n"),

%     <<_:8, Length:24, _:32, _/binary>> = <<Header:160>>,
%     % io:format("== Length is: ~w~n~n", [Length]),

%     Length_Bit = Length * 8,
%     % io:format("== Length_Bit is: ~w~n~n", [Length_Bit]),

%     Real_Length_Bit = bit_size(Bin),
%     % io:format("== Real_Length_Bit is: ~w~n~n", [Real_Length_Bit]),

%     % io:format("=+=+=+=+=+=+=+=+=+=+=+=+= ~n~n~n"),

%     case Real_Length_Bit < Length_Bit of 
%         true ->
%             % io:format("Smaller ~n~n"),
%             Rest = <<>>,
%             TOSEND = Bin;
%         false ->
%             % io:format("Bigger ~n~n"),
%             <<Packet:Length_Bit/bitstring, Rest/bitstring>> = Bin,
%             Data = mprocess_data(<<Packet:Length_Bit/bitstring>>),
%             % io:format("What is +DATA+ here ~w~n", [Data]),
%             TOSEND = Data
%     end,

%     % gen_tcp:send(Client, TOSEND),
%     % inet:setopts(Server, [{active, once}]),
%     % {noreply, State},

%     process_data(Rest);


% process_data(TOSEND) ->
%     % io:format("this is Fragmented data: ~p~n", [TOSEND]),
%     % gen_tcp:send(Client, TOSEND),
%     % inet:setopts(Server, [{active, once}]),
%     % {noreply, State}.
%     [].
% %%%%%%%

%%%%%%%
mprocess_data(<<Bin/binary>>) ->
    Data = <<Bin/binary>>,

    % % Decode AVPs
    <<Header:160, Rest/binary>> = Bin,

    % <<Version:8, PacketLength:24, R:1, P:1, E:1, T:1, Reserved:4, Command:24, Application_id:32, H_by_hop_ID:32, E_to_E_ID:32>> = <<Header:160>>,
    <<_:8, PacketLength:24, _:32, _:32, _:32, _:32>> = <<Header:160>>,

    HeaderByte = 20,
    AVPLengthBit = ((PacketLength - HeaderByte) * 8),

    <<AVPBIN:AVPLengthBit/bitstring>> = <<Rest/binary>>,


    AVPs = diameter_codec:decode_avps(AVPBIN),
    % io:format("Final AVPs are here ~w~n", [AVPs]),

    % AVPs_dict0 = dict:new(),
    AVPs_dict = dict:from_list(AVPs),
    % io:format("AVPs_dict are here ~w~n", [AVPs_dict]), 

    % % Look for CC-Request-Type value = 1 (CCR-I)
    % Entry_416 = dict:find(416, AVPs_dict),  
    % % io:format("Entry_416 is: ~w~n", [Entry_416]), 

    % % Look for Result-Code, 268
    % Entry_268 = dict:find(268, AVPs_dict),
    % % io:format("Entry_268 is: ~w~n", [Entry_268]), 

    % % Look for MSCC 4012
    % Entry_456 = dict:find(456, AVPs_dict),
    % % io:format("Entry_456 is: ~w~n", [Entry_456]), 
    % % io:format("Multipass is: ~w~n", [[Entry_416, Entry_268, Entry_456]]), 

    Filter = check_filter([dict:find(416, AVPs_dict), dict:find(268, AVPs_dict), dict:find(456, AVPs_dict)]),
    % io:format("check_filter is: ~w~n", [Filter]), 

    case Filter of 
        {ok, true} ->
            % io:format("RED ~n"),
            ENC_AVPs_List = diameter_codec:encode_avps(AVPs),
            % io:format("ENC_AVPs_List value is: ~p~n", [ENC_AVPs_List]),
            % io:format("Header value is: ~p~n", [<<Header:160>>]),

            ENC_AVPs_flat = lists:flatten(ENC_AVPs_List),
            % io:format("SFLAT is now ~w~n", [ENC_AVPs_flat]),

            _AVP = list_to_binary(ENC_AVPs_flat),
            _Head = <<Header:160>>,
            TOSEND = <<_Head/binary, _AVP/binary>>;
            % io:format("TOSEND is now ~w~n", [TOSEND]);
        {ok, false} ->
            % io:format("Green ~n"),
            TOSEND = Data            
    end,
    TOSEND.


check_filter([{ok, AVP_416}, {ok, AVP_268}, {ok, AVP_456}]) ->   
    % io:format("check_filter 1 ok ~n"), 
    % io:format("AVP_416: ~w~n", [AVP_416#diameter_avp.value]),
    % io:format("AVP_268: ~w~n", [AVP_268#diameter_avp.value]),
    % io:format("AVP_456: ~w~n", [AVP_456#diameter_avp.value]),

    check_filter(AVP_416#diameter_avp.value, AVP_268#diameter_avp.value, AVP_456);

check_filter(_) ->
    % io:format("Got bad values ~n"),
    {ok, false}.

check_filter(1, 2001, AVP_456) ->
    % io:format("check_filter 2 ok ~n"), 

    % io:format("Got good values ~n"),
    MSCC_dict = dict:from_list(AVP_456#diameter_avp.value),
    % io:format("MSCC_dict_new is: ~w~n", [MSCC_dict]), 

    % Look for 268 in MSCC
    MSCC_268_find = dict:find(268, MSCC_dict),
    % io:format("MSCC_268_new is: ~w~n", [MSCC_268_find]),

    case MSCC_268_find of 
        {ok, MSCC_268} ->
            <<Value:32>> = MSCC_268#diameter_avp.value;
            % io:format("MSCC_268 value is: ~w~n", [MSCC_268#diameter_avp.value]);
        _ -> 
            % io:format("MSCC_268 we got 0"),
            Value = 0
    end,

    % io:format("MSCC_268_new value is: ~w~n", [Value]),

    check_filter_mscc(Value);

check_filter(2, 2001, AVP_456) ->
    % io:format("check_filter 2 ok ~n"), 

    % io:format("Got good values ~n"),
    MSCC_dict = dict:from_list(AVP_456#diameter_avp.value),
    % io:format("MSCC_dict_new is: ~w~n", [MSCC_dict]), 

    % Look for 268 in MSCC
    MSCC_268_find = dict:find(268, MSCC_dict),
    % io:format("MSCC_268_new is: ~w~n", [MSCC_268_find]),

    case MSCC_268_find of 
        {ok, MSCC_268} ->
            <<Value:32>> = MSCC_268#diameter_avp.value;
            % io:format("MSCC_268 value is: ~w~n", [MSCC_268#diameter_avp.value]);
        _ -> 
            % io:format("MSCC_268 we got 0"),
            Value = 0
    end,

    % io:format("MSCC_268_new value is: ~w~n", [Value]),

    check_filter_mscc(Value);


check_filter(_, _, _) ->
    % io:format("Got bad values ~n"),
    {ok, false}.



% Check MSCCs
check_filter_mscc(4012) ->
    % io:format("P is 4012 ~n"),
    {ok, true};

check_filter_mscc(_) ->
    % io:format("Got MSCC bad values ~n"),
    {ok, false}.