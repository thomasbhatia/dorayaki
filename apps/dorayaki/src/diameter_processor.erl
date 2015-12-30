%%%-------------------------------------------------------------------
%% @doc dorayaki diameter_processor public API
%% @end
%%%-------------------------------------------------------------------

-module(diameter_processor).

-define(APPLICATION, dorayaki).

-export([diameter_process/2]).

-include("diameter.hrl").

%find response3 sitting in State#state.resp
-record(state, {client, server}).

diameter_process(Bin, Var) ->
	io:format("bin is: ~p~n", [Bin]),
	preprocess_data(Bin, Var).

%%====================================================================
%% Internal functions
%%====================================================================

%%%%%%% Sanitize 
preprocess_data(<<Version:8, Length:24, Flags:8, Command:24, T/binary>>, {Client, Server, State})->
    Data = <<Version:8, Length:24, Flags:8, Command:24, T/binary>>,
    % io:format("this is data for test: ~p~n", [Data]),

    case Command =:= 272 of 
        true ->
            % io:format("It's 272: ~n"),
            process_data(Data, {Client, Server, State});
            
        false ->
            % io:format("Not 272 ~n"),
            gen_tcp:send(Client, Data),
            inet:setopts(Server, [{active, once}]),
            {noreply, State}
    end;

preprocess_data(Data , {Client, Server, State})->
    % io:format("this is BAD data: ~p~n", [Data]),
    gen_tcp:send(Client, Data),
    inet:setopts(Server, [{active, once}]),
    {noreply, State}.
%%%%%%%

%%%%%%% process
process_data(<<Header:160 , Body/binary>>, {Client, Server, State}) ->
    % io:format("Bin: ~w~n", [Bin]),
    Bin = <<Header:160 , Body/binary>>,

    % io:format("=+=+=+=+=+=+=+=+=+=+=+=+= ~n~n~n"),

    <<_:8, Length:24, _:32, _/binary>> = <<Header:160>>,
    % io:format("== Length is: ~w~n~n", [Length]),

    Length_Bit = Length * 8,
    % io:format("== Length_Bit is: ~w~n~n", [Length_Bit]),

    Real_Length_Bit = bit_size(Bin),
    % io:format("== Real_Length_Bit is: ~w~n~n", [Real_Length_Bit]),

    % io:format("=+=+=+=+=+=+=+=+=+=+=+=+= ~n~n~n"),

    case Real_Length_Bit < Length_Bit of 
        true ->
            % io:format("Smaller ~n~n"),
            Rest = <<>>,
            TOSEND = Bin;
        false ->
            % io:format("Bigger ~n~n"),
            <<Packet:Length_Bit/bitstring, Rest/bitstring>> = Bin,
            Data = mprocess_data(<<Packet:Length_Bit/bitstring>>),
            % io:format("What is +DATA+ here ~w~n", [Data]),
            TOSEND = Data
    end,

    gen_tcp:send(Client, TOSEND),
    inet:setopts(Server, [{active, once}]),
    {noreply, State},

    process_data(Rest, {Client, Server, State});


process_data(TOSEND, {Client, Server, State}) ->
    % io:format("this is Fragmented data: ~p~n", [TOSEND]),
    gen_tcp:send(Client, TOSEND),
    inet:setopts(Server, [{active, once}]),
    {noreply, State}.
%%%%%%%

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