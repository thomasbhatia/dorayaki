%%%-------------------------------------------------------------------
%% @doc dorayaki top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('dorayaki_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    CLIENT_PORT = get_client_port(),
    io:format("CLIENT_PORT at start_link ~p~n", [CLIENT_PORT]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [CLIENT_PORT]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}

init([CLIENT_PORT]) ->
    % {ok, { {one_for_all, 0, 1}, []} }.
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 3600,
 
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
 
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {dorayaki_client, {dorayaki_client, start, [CLIENT_PORT]},
          Restart, Shutdown, Type, [dorayaki_client]},
 
    {ok, {SupFlags, [AChild]}}.
    
%%====================================================================
%% Internal functions
%%====================================================================

get_client_port() ->
    P = config_loader:get_env(client_port),
    case P of 
        {ok, Port} -> Port;
    Port -> 
        io:format("P ~p~n", [Port]),
        Port
    end.