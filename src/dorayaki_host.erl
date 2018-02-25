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


%%%-------------------------------------------------------------------
%% @doc handle connection setup
%% @end
%%%-------------------------------------------------------------------
-spec handle_cast(setup_socket, 
                #state{client::port()}) -> {noreply, 
                                            #state{client::port(), server::port()}} | {stop, _, #state{client::port()}}.
handle_cast(setup_socket, #state{client=Client}=State) ->
    lager:log(info, console, "#############################"),
    lager:log(info, console, "Connecting to Host at IP ~p on port ~p..... ", [?HOST_IP, ?HOST_PORT]),

    inet:setopts(Client, [{active, once}]),
    case gen_tcp:connect(?HOST_IP, ?HOST_PORT, [binary, {active, once}, {packet, 0}]) of
        {ok, Server} ->
            lager:log(info, console, "Now connected to OCS"),
            lager:log(info, console, "#############################"),
            {noreply, #state{client=Client, server=Server}};
        Error ->
            lager:log(error, console, "Error connecting to OCS"),
            {stop, lager:log(info, console, "Relay exception: ~p", [Error]), State}
    end.

%%%-------------------------------------------------------------------
%% @doc handle connection termination
%% @end
%%%-------------------------------------------------------------------
-spec handle_info({tcp_closed, _} |
                  {tcp, 
                    port(), 
                    binary() | 
                    maybe_improper_list(binary() | maybe_improper_list(any(), binary() | []) | byte(), binary() | []) | 
                    integer()}, 
                  #state{}) -> {noreply, #state{server::port()}} | {stop, shutdown, #state{}}.
handle_info({tcp_closed, Socket}, #state{client=Client, server=Server}=State) ->
    case Socket of
        Client ->
            gen_tcp:close(Server);
        Server ->
            gen_tcp:close(Client)
    end,
    {stop, shutdown, State};

%%%-------------------------------------------------------------------
%% @doc handle connection from Client (GGSN)
%% @end
%%%-------------------------------------------------------------------
handle_info({tcp, Client, Data}, #state{client=Client, server=Server}=State) ->
    gen_tcp:send(Server, Data),
            inet:setopts(Client, [{active, once}]),
            {noreply, State};

%%%-------------------------------------------------------------------
%% @doc handle connection from Server (OCS)
%% @end
%%%-------------------------------------------------------------------
handle_info({tcp, _Server, Bin}, State) ->
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
    dorayaki_diameter_processor:process_packet(Bin, State);

check_data_integrity(Bin = <<_Version:8, Length:24, _Payload/binary>>, State) when Length < size(Bin) ->
    Length_bit = Length*8,
    <<Bin2:Length_bit, Rest/binary>> = Bin,
    Bin3 = <<Bin2:Length_bit>>,
    Bin4 = <<Rest/binary>>,
    check_data_integrity(Bin3, State),
    check_data_integrity(Bin4, State);

check_data_integrity(Bin = <<_Version:8, Length:24, _Payload/binary>>, State) when Length > size(Bin) ->
    case gen_tcp:recv(State#state.server, 0, ?TIMEOUT) of
        {ok, NextBinList} ->
            io:format("got more data from sock~w~n", [NextBinList]),
            NextBin = list_to_binary(NextBinList),
            NewBin = <<Bin/binary, NextBin/binary>>,
            check_data_integrity(NewBin, State);
        {error, timeout} ->
            Bin;
        {error, _Reason} ->
            Bin
    end;
check_data_integrity(_Bin, State) ->
    %% discard fragment
    inet:setopts(State#state.server, [{active, once}]),
    {noreply, State}.




