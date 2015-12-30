-module('dorayaki_host').
 
-behaviour(gen_server).
 
-define(HOST_IP, config_loader:get_env(host_ip)).
-define(HOST_PORT, config_loader:get_env(host_port)).

-export([start/1]).
 
%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2, handle_call/3,
         terminate/2, code_change/3]).
 
-define(SERVER, ?MODULE).

-include("diameter.hrl").

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
    % io:format("this is data from GGSN: ~p~n", [Data]), 
    inet:setopts(Client, [{active, once}]),
    {noreply, State};

% From Server (OCS)
handle_info({tcp, Server, Data}, #state{client=Client, server=Server}=State) ->
    % preprocess_data(Data, {Client, Server, State}).
    diameter_processor:diameter_process(Data, {Client, Server, State}).

% Doesn't do anything
handle_call(_,_,_) -> {ok, undefined}.


terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

