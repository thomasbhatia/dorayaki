%%%-------------------------------------------------------------------
%% @doc diameter_code public API
%% @end
%%%-------------------------------------------------------------------

-module(diameter_codec).

% -include("diameter.hrl").
-include_lib("dorayaki/include/diameter.hrl").

-export([decode_header/1, decode_avps/1, encode_avps/1]).

%%====================================================================
%% API
%%====================================================================

% Decode Diameter header function
decode_header(<<Version:8, Length:24, R:1, P:1, E:1, T:1, Reserved:4, Command:24, Application_id:32, H_by_hop_ID:32, E_to_E_ID:32, Bin/binary>>) ->
    % io:format("It's version ~w~n", [Version]),
    % io:format("It's Length ~w~n", [Length]),
    % io:format("It's Command ~w~n", [Command]),
    % io:format("It's Application_id ~w~n", [Application_id]),
    % io:format("It's H_by_hop_ID ~w~n", [H_by_hop_ID]),
    % io:format("It's E_to_E_ID ~w~n", [E_to_E_ID]),

    Data_bits = (Length * 8) - 160,
    % io:format("Data_bits is: ~w~n~n", [Data_bits]),

    <<Diameter_body_bin:Data_bits/bitstring, Rest/binary>> = Bin,
    % io:format("Diameter_body_bin is: ~p~n", [Diameter_body_bin]),
    % io:format("Rest ~w~n", [Rest]),
    
    Header = #diameter_header{
            version=Version, 
            length=Length, 
            cmd_code=Command, 
            application_id=Application_id, 
            hop_by_hop_id=H_by_hop_ID,
            end_to_end_id=E_to_E_ID,
            is_request=R,
            is_proxiable=P,
            is_error=E,
            is_retransmitted=T,
            data = <<Version:8, Length:24, R:1, P:1, E:1, T:1, Reserved:4, Command:24, Application_id:32, H_by_hop_ID:32, E_to_E_ID:32>>
            },

    % io:format("Header is: ~p~n", [Header]),
    {Header, Diameter_body_bin, Rest}.

% Decode Diameter AVP function
decode_avps(Bin) ->
    AVPs = [],
    decode_avps(Bin, AVPs, []).

decode_avps(<<Code:32, V:1, M:1, P:1, Null:5, AVP_length:24, Rest/binary>>, AVPs, []) ->
    % % Step 1. Get AVP_length
    % io:format("========================~n"), 
    % io:format("Code is: ~w~n", [Code]), 
    % % Step 2. Get AVP.
    % io:format("AVP_length is: ~w~n", [AVP_length]),
    % % Get padding
    Pad = get_padding(Code, AVP_length),
    % io:format("Pad is: ~w~n", [Pad]),

    Data_bits = (AVP_length * 8) - 64,
    % io:format("Data_bits is: ~w~n", [Data_bits]),

    case Pad of 
        0 ->
            % Padding = 0,
            <<Data:Data_bits/bitstring, Rest2/binary>> = Rest,
            New_AVP = <<Code:32, V:1, M:1, P:1, Null:5, AVP_length:24, Data:Data_bits/bitstring>>;
        _ ->
            <<Data:Data_bits/bitstring, Padding:Pad, Rest2/binary>> = Rest,
            New_AVP = <<Code:32, V:1, M:1, P:1, Null:5, AVP_length:24, Data:Data_bits/bitstring, Padding:Pad>>
    end,

    % Decode 
    % Name
    %{_, Name} = avp_mapper:num_to_name(Code), 
    % io:format("Got name is: ~w~n", [Name]), 
    
    % Value
    case Code of
        456 -> 
            % io:format("CODE IS 456 ~n"), 
            <<BinValue/binary>> = Data,
            Value = decode_456(BinValue);
            % io:format("456 Value is: ~w~n", [Value]);
        _ ->
            % io:format("Not 456, pass ~n"),
            <<Value:Data_bits>> = Data
    end, 

    % io:format("Data is: ~w~n", [Value]),

    % Build records
    AVP0 = #diameter_avp{
             code = Code,
             vendor_id = '',
             v = V,
             m = M,
             p = P,
             % Don't forget Null = 5
             length = AVP_length,
             data = New_AVP,
             % name = Name,
             value = Value,
             type = '',
             padding = Pad
            },

    AVP = {Code, AVP0},

    % io:format("AVP record is: ~p~n", [AVP]),
    % io:format("Is there more AVP? ~w~n", [Rest2]), 

    decode_avps(Rest2, [AVP|AVPs], []);

decode_avps(_, AVPs, []) ->
    % io:format("end of road, no bins ~n"),
    AVPs.


encode_avps(AVPs) ->
    % io:format("Flipin ~n"),
    % io:format("Flipin AVPs ~w~n", [AVPs]),
    BinList = [],
    encode_avps([AVPs], [BinList], []).

encode_avps([AVPs], [BinList], []) ->
    % io:format("Flipin again ~n"),
    % io:format("This is AVPs: ~w~n", [AVPs]),
    % io:format("This is BinList: ~w~n", [BinList]),

    [H] = lists:sublist(AVPs,1),
    % io:format("This is head: ~w~n", [H]),

    NewAVPs = lists:delete(H, AVPs),
    % io:format("This is NewAVPs: ~w~n", [NewAVPs]),

    {_, R} = H,
    % io:format("This is S: ~w~n", [R]),

    case R#diameter_avp.code of 
        268 ->
            % Changed from 4012 to 5030
            R0 = <<0,0,1,12,64,0,0,12,0,0,19,166>>; 
            % io:format("This is R0: ~p~n", [R0]),

            % io:format("Compare to original: ~w~n", [R#diameter_avp.data]);
        _ ->
            R0=R#diameter_avp.data
            % io:format("This is R0: ~p~n", [R0])
    end,

    RList = binary_to_list(R0), 
    % io:format("This is RList: ~p~n", [RList]),

    NBinList = [RList|BinList],
    % io:format("This is NBinList: ~w~n", [NBinList]),

    case NewAVPs of 
        [] -> 
            % io:format("Yes ~n") ,
            encode_avps([NBinList], []);
        _ -> 
            % io:format("This is All NewAVPs: ~w~n", [NewAVPs]),
            % io:format("No ~n"),
            encode_avps([NewAVPs], [NBinList], [])
    end.

encode_avps([NBinList], []) ->
    % io:format("NBinList is now ~w~n", [NBinList]),
    [NBinList].

%%====================================================================
%% Internal functions
%%====================================================================

% Decode MSCC - 456
decode_456(Bin) ->
    % io:format("Is there bin: ~w~n", [Bin]),
    MSCC = [],
    decode_456(Bin, MSCC, []).

decode_456(<<AVP_456_Code:32, V:1, M:1, P:1, Null:5, AVP_456_Length:24, MSCC_REST/binary>>, MSCC, []) ->
    % io:format("Is there MSCC: ~w~n", [MSCC]),

    MSCC_Data_bits = (AVP_456_Length * 8) - 64,
    % io:format("MSCC_Data_bits: ~w~n", [MSCC_Data_bits]),

    <<MSCC_DATA:MSCC_Data_bits/bitstring, MSCC_REST2/binary>> = MSCC_REST,
    % io:format("MSCC_DATA: ~w~n", [MSCC_DATA]),

    NEW_MSCC = <<AVP_456_Code:32, V:1, M:1, P:1, Null:5, AVP_456_Length:24, MSCC_DATA:MSCC_Data_bits/bitstring>>,

    % Decode 
    % Name
    %{_, Name} = avp_mapper:num_to_name(AVP_456_Code), 
    % io:format("Got new name is: ~w~n", [Name]), 

    % Build records
    AVP0 = #diameter_avp{
             code = AVP_456_Code,
             vendor_id = '',
             v = V,
             m = M,
             p = P,
             % Don't forget Null = 5
             length = AVP_456_Length,
             data = NEW_MSCC,
             % name = Name,
             value = MSCC_DATA,
             type = '',
             padding = ''
            },
    AVP = {AVP_456_Code, AVP0},

    % io:format("Got new AVP is: ~w~n", [AVP]), 
    % io:format("And the MSCC is: ~w~n", [MSCC]), 
    % io:format("Got new MSCC_REST2 is: ~w~n", [MSCC_REST2]), 
    % io:format("==============%============= ~n"),

    decode_456(MSCC_REST2, [AVP|MSCC], []);

decode_456(<<>>, MSCC, []) ->
    % io:format("No more Bin ~n"),
    % io:format("MSCC is end at: ~w~n", [MSCC]),
    % io:format("++++++++++++++++++++++++++++~n"),
    MSCC.

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

    


