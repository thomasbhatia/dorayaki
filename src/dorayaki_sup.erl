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
%% @doc Dorayaki top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('dorayaki_sup').
-copyright('Copyright (c) 2016 Thomas Bhatia').
-author('thomas.bhatia@eo.io').

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
    ok = dorayaki_config_loader:load_config(),
    {ok, CLIENT_PORT} = application:get_env(dorayaki, client_port),

    lager:log(debug, console, "CLIENT_PORT at start_link ~p", [CLIENT_PORT]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [CLIENT_PORT]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([CLIENT_PORT]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Child = [{dorayaki_client, 
             {dorayaki_client, start, [CLIENT_PORT]},
             Restart, 
             Shutdown, 
             Type, 
             [dorayaki_client]}],

    {ok, {SupFlags, Child}}.

%%====================================================================
%% Internal functions
%%====================================================================
