%%%-------------------------------------------------------------------
%% @doc Dorayaki host public API
%% @end
%%%-------------------------------------------------------------------

-module('dorayaki_host').
-copyright('Copyright (c) 2016 Thomas Bhatia').
-author('thomas.bhatia@eo.io').

-behaviour(gen_server).
 
-define(HOST_IP, dorayaki_config_loader:get_env(host_ip)).
-define(HOST_PORT, dorayaki_config_loader:get_env(host_port)).

-export([start/1]).
 
%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2, handle_call/3,
         terminate/2, code_change/3]).
 
-define(SERVER, ?MODULE).

-define(TIMEOUT, 0).

-record(state, {client, server}).

-spec start(port()) -> {'ok',pid()}.
start(Client) ->
    {ok, Pid} = gen_server:start(?MODULE, Client, []),
    gen_tcp:controlling_process(Client, Pid),
    gen_server:cast(Pid, setup_socket),
    {ok, Pid}.
 
-spec init(_) -> {'ok',#state{}}.
init(Client) ->
    {ok, #state{client=Client}}.
 
 
-spec handle_cast('setup_socket',#state{client::port()}) -> {'noreply',#state{client::port(),server::port()}} | {'stop',_,#state{client::port()}}.
handle_cast(setup_socket, #state{client=Client}=State) ->
    lager:log(info, "console", "#############################"),
    lager:log(info, "console", "Connecting to Host at IP ~p on port ~p..... ", [?HOST_IP, ?HOST_PORT]),

    inet:setopts(Client, [{active, once}]),
    case gen_tcp:connect(?HOST_IP, ?HOST_PORT, [binary, {active, once}, {packet, 0}]) of
        {ok, Server} ->
            lager:log(info, "console", "Now connected to OCS"),
            lager:log(info, "console", "#############################"),
            {noreply, #state{client=Client, server=Server}};
        Error ->
            lager:log(error, "console", "Error connecting to OCS"),
            {stop, lager:log(info, "console", "Relay exception: ~p", [Error]), State}
    end.
 
% handle connection termination
-spec handle_info({'tcp_closed',_} | {'tcp',port(),binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | []) | integer()},#state{}) -> <<_:32,_:_*8>> | {'noreply',#state{server::port()}} | {'stop','shutdown',#state{}}.
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
    lager:log(debug, "console", "Received data from GGSN: ~w", [Data]),
    gen_tcp:send(Server, Data),
            inet:setopts(Client, [{active, once}]),
            {noreply, State};

% From Server (OCS)
handle_info({tcp, Server, Bin}, #state{client=Client, server=Server}=State) ->
    lager:log(debug, "console", "Client is: ~w", [Client]),
    lager:log(debug, "console", "Server is: ~w", [Server]),
    lager:log(debug, "console", "State is: ~w", [State]),
    lager:log(debug, "console", "Received data from OCS: ~w", [Bin]),
    check_data_integrity(Bin, State).

% Doesn't do anything
-spec handle_call(_,_,_) -> {'noreply','undefined'}.
handle_call(_,_,_) -> 
    {noreply, undefined}.

-spec terminate(_,_) -> 'ok'.
terminate(_Reason, _State) ->
    ok.
 
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
-spec check_data_integrity(<<_:32,_:_*8>> | integer(),#state{server::port()}) -> <<_:32,_:_*8>> | {'noreply',#state{server::port()}}.
check_data_integrity(Bin = <<_Version:8, Length:24, _Payload/binary>>, State) when Length =:= size(Bin) -> 
    lager:log(debug, "console", "CHECK 1."),
    lager:log(debug, "console", "Size of bin: ~w", [size(Bin)]),
    lager:log(debug, "console", "Length: ~w", [Length]),
    lager:log(debug, "console", "Bin is: ~w", [Bin]),
    dorayaki_diameter_processor:process_packet(Bin, State);

check_data_integrity(Bin = <<_Version:8, Length:24, _Payload/binary>>, State) when Length < size(Bin) -> 
    lager:log(debug, "console", "CHECK 2."),
    lager:log(debug, "console", "Size of bin: ~w", [size(Bin)]),
    lager:log(debug, "console", "Length: ~w", [Length]),
    Length_bit = Length*8,
    lager:log(debug, "console", "Length_bit is: ~w", [Length_bit]),
    <<Bin2:Length_bit, Rest/binary>> = Bin, 
    Bin3 = <<Bin2:Length_bit>>,
    Bin4 = <<Rest/binary>>,
    lager:log(debug, "console", "Bin3 is: ~w", [Bin3]),
    lager:log(debug, "console", "Size of Rest: ~w", [Bin4]),
    check_data_integrity(Bin3, State),
    check_data_integrity(Bin4, State);

check_data_integrity(Bin = <<_Version:8, Length:24, _Payload/binary>>, State) when Length > size(Bin) -> 
    io:format("3. ~n"),
    io:format("Size of bin ~p~n", [size(Bin)]),
    io:format("Length ~p~n", [Length]),
    io:format("Bin ~p~n", [Bin]),
    case gen_tcp:recv(State#state.server, 0, ?TIMEOUT) of
        {ok, NextBinList} ->   
            io:format("got more data from sock~w~n", [NextBinList]),
            NextBin = list_to_binary(NextBinList),
            NewBin = <<Bin/binary, NextBin/binary>>,
            check_data_integrity(NewBin, State);
        {error, timeout} ->
            io:format("got timeout~n"),
            Bin;
        {error, Reason} ->
            io:format("Errrrrooooo!! ~p~n", [Reason]),
            Bin
    end;
    

check_data_integrity(Bin, State) -> 
    lager:log(debug, "console", "CHECK 4."),
    lager:log(debug, "console", "Bin: ~w", [Bin]),
    lager:log(debug, "console", "Size of bin: ~w", [size(<<Bin>>)]),

    %% discard fragment
    inet:setopts(State#state.server, [{active, once}]),
    {noreply, State}.




