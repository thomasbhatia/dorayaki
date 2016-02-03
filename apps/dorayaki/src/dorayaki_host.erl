-module('dorayaki_host').
 
-behaviour(gen_server).
 
-define(HOST_IP, config_loader:get_env(host_ip)).
-define(HOST_PORT, config_loader:get_env(host_port)).

-export([start/1]).
 
%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2, handle_call/3,
         terminate/2, code_change/3]).
 
-define(SERVER, ?MODULE).

% -define(TIMEOUT, 0).

%find response3 sitting in State#state.resp
-record(state, {client, server}).

start(Client) ->
    {ok, Pid} = gen_server:start(?MODULE, Client, []),
    gen_tcp:controlling_process(Client, Pid),
    gen_server:cast(Pid, setup_socket),
    {ok, Pid}.
 
init(Client) ->
    {ok, #state{client=Client}}.
 
 
handle_cast(setup_socket, #state{client=Client}=State) ->
    io:format("########################################~n"),
    io:format("Connecting to Host at IP ~p on port ~p..... ~n", [?HOST_IP, ?HOST_PORT]), 

    inet:setopts(Client, [{active, once}]),
    case gen_tcp:connect(?HOST_IP, ?HOST_PORT, [binary, {active, once}, {packet, 0}]) of
        {ok, Server} ->
            io:format("Now connected to OCS ~n"),
            io:format("########################################~n"),
            {noreply, #state{client=Client, server=Server}};
        Error ->
            io:format("Error connecting to OCS ~n"),
            {stop, io_lib:format("Relay exception: ~p~n", [Error]), State}
    end.
 
% handle connection termination
handle_info({tcp_closed, Socket}, #state{client=Client, server=Server}=State) ->
    case Socket of
        Client ->
            gen_tcp:close(Server);
        Server ->
            gen_tcp:close(Client)
    end,
    {stop, shutdown, State};
  
% From Client (GGSN)
handle_info({tcp, Client, Data}, #state{client=Client, server=Server}=State) ->
    gen_tcp:send(Server, Data),
            inet:setopts(Client, [{active, once}]),
            {noreply, State};

% From Server (OCS)
handle_info({tcp, Server, Data}, #state{client=Client, server=Server}=State) ->
    case diameter_processor:process_packet(Data) of 
        [] -> 
            OutData = Data;

        [NewData] ->
            OutData = NewData
    end,

    gen_tcp:send(Client, Data),
    inet:setopts(Server, [{active, once}]),
    {noreply, State}.

% Doesn't do anything
handle_call(_,_,_) -> {ok, undefined}.


terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

% check_bin_length(<<Version:8, Length:24, _/bitstring>>) ->


% do_stat(Sock, <<Version:8, Length:24, Payload/binary>>) ->
%     Bin = <<Version:8, Length:24, Payload/bitstring>>,
%     io:format("sizeof Bin is ~p~n", [bit_size(Bin)]),
%     io:format("Length is ~p~n", [Length*8]),
%     case (bit_size(Bin) < Length*8) of
%         false ->
%             io:format("got right size or bigger ~n");
%         true ->
%             io:format("wtf??? ~n")
%     end;

% do_stat(Sock,  <<_>> = Bin) ->
%     io:format("Bin is ~p~n", [Bin]),
%     case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
%         {ok, BinList} ->   
%             io:format("got more data from sock~w~n", [BinList]);
%             % NewBin = list_to_binary(BinList),
%             % NewBin = <<Bin/binary, NewBin/binary>>,
%             % NewBin;
%         {error, timeout} ->
%             io:format("got timeout~n"),
%             Bin;
%         {error, Reason} ->
%             io:format("Errrrrooooo!! ~n"),
%             exit(Reason)
%     end;

% do_stat(Sock,  _ = BinList) ->
%     io:format("BinList is ~p~n", [BinList]),
%     do_stat(Sock, list_to_binary(BinList)).

% do_recv(Sock, Gathered) ->
%   case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
%     {ok, BinList} ->
%       io:format("got more data from sock as list ~w~n", [BinList]),
%       Bin = list_to_binary(BinList),
%       io:format("changed to bin ~w~n", [Bin]),
%       % Remaining = accsend(Sock, <<Gathered/binary, Bin/binary>>),
%       Remaining = <<Gathered/binary, Bin/binary>>,
%       io:format("Got Remaining!! ~n"),
%       do_recv(Sock, Remaining);
%     {error, timeout} ->
%     io:format("got timeout~n");
%       % do_recv(Sock, Remaining);
%     {_A, _B} ->
%       io:format("got something, A ~p~n", [_A]),
%       io:format("got something, B ~p~n", [_B]);
%     {error, Reason} ->
%       io:format("Errrrrooooo!! ~n"),
%       exit(Reason)
%    end.

% accsend(Sock, Bin) ->
%     {ok, Bin}.

%  try_decode(Sock, Gathered) ->
%    case decode(Gathered) of
%       {ok, Data} ->
%          % processor ! Data,
%          % try_decode(Sock, Rest);
%          io:format("Got all data!! ~n"),
%          Data;
%       need_more_data ->
%          io:format("Need more data!! ~n"),
%          do_recv(Sock, Gathered)
%    end.


% decode(Bin) ->
%     <<_Version:8, Length:24, _/binary>> = Bin,
%     io:format("Length is ~p~n", [Length]),
%     Length_Bit = Length * 8,
%     io:format("== Length_Bit is: ~w~n", [Length_Bit]),
%     Real_Length_Bit = bit_size(Bin),
%     io:format("== Length_Bit is: ~w~n", [Real_Length_Bit]),

%     case Length_Bit =:= Real_Length_Bit of
%         true ->
%             {ok, Bin};
%         false ->
%             need_more_data
%     end.



