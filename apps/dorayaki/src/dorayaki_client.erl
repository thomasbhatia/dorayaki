-module('dorayaki_client').
 
-export([start_link/1, start/1, init/1]).

start_link(Port) ->
    proc_lib:start_link(?MODULE, init, [{self(), Port}]).
 
init({Parent, Port}) ->
    register(?MODULE, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    {ok, ListenSock} = gen_tcp:listen(Port, [{packet, 0}, {active, false}, {reuseaddr, true}]),
    lager:log(info, "console", "Module now accepting connections from Diameter clients on port ~p", [Port]),

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
    lager:log(info, "console", "The client port initialising on ~p", [Port]),
    start_link(Port).