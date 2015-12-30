-module('dorayaki_client').
 
-export([start_link/1, start/1, init/1]).
 
% -define(MAX_LINE_SIZE, 1820).
% -define(TCP_OPTIONS, [binary, {packet, 0}, {packet_size, 1820}, {active, once}, {reuseaddr, true}, {recbuf, ?MAX_LINE_SIZE}]).

start_link(Port) ->
    proc_lib:start_link(?MODULE, init, [{self(), Port}]).
 
init({Parent, Port}) ->
    register(?MODULE, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    {ok, ListenSock} = gen_tcp:listen(Port, [{packet, 0}, {active, false}, {reuseaddr, true}]),
    io:format("Module now accepting connections from Diameter clients on port ~p~n", [Port]), 
    accept(ListenSock).
 
accept(ListenSock) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Client} ->
            {ok, _} = dorayaki_host:start(Client),
            accept(ListenSock);
        _Error ->
            ok
    end.
 
start(Port) ->
    io:format("The client port initialising on ~p~n", [Port]),
    start_link(Port).