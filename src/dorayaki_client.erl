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
%% @doc Dorayaki client public API
%% @end
%%%-------------------------------------------------------------------

-module('dorayaki_client').
-copyright('Copyright (c) 2016 Thomas Bhatia').
-author('thomas.bhatia@eo.io').

-export([start_link/1, start/1, init/1]).

-spec start_link(_) -> any().
start_link(Port) ->
    proc_lib:start_link(?MODULE, init, [{self(), Port}]).

-spec init({pid(), char()}) -> ok.
init({Parent, Port}) ->
    register(?MODULE, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    {ok, ListenSock} = gen_tcp:listen(Port, [{packet, 0}, {active, false}, {reuseaddr, true}]),
    lager:log(info, console, "Module now accepting connections from Diameter clients on port ~p", [Port]),

    accept(ListenSock).

-spec accept(port()) -> ok.
accept(ListenSock) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Client} ->
            {ok, _} = dorayaki_host:start(Client),
            accept(ListenSock);
        _Error ->
            ok
    end.

start(Port) ->
    lager:log(info, console, "The client port initialising on ~p", [Port]),
    start_link(Port).
