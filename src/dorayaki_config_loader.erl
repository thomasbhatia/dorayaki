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
%% @doc Dorayaki config_loader public API
%% @end
%%%-------------------------------------------------------------------

-module(dorayaki_config_loader).


-ifdef(TEST).
-define(CONFIG_FILE, "dorayaki_test.cfg").
-else.
-define(CONFIG_FILE, "dorayaki.cfg").
-endif.

-export([get_env/1,
        load_config/0]).


%%%-------------------------------------------------------------------
%% @doc
%% @end
%%%-------------------------------------------------------------------

-spec get_env(atom()) -> any().

get_env(Key) ->
    {ok, Value} = application:get_env(dorayaki, Key),
    Value.

-spec load_config() -> ok.
load_config() ->
    {ok, Config_path} = get_config_path(),
    {ok, ConfigList} = file:consult(Config_path),
    do_load_config(ConfigList).

-spec do_load_config([{atom(), _}]) -> ok.
do_load_config([{ConfigName, ConfigValue}|RestConfig]) when ConfigName =:= log ->
    Log_level = proplists:get_value(level, ConfigValue),
    lager:set_loglevel(lager_console_backend, Log_level),
    lager:set_loglevel(lager_file_backend, "console.log", Log_level),
    do_load_config(RestConfig);
do_load_config([{ConfigName, ConfigValue}|RestConfig]) ->
    application:set_env(dorayaki, ConfigName, ConfigValue),
    do_load_config(RestConfig);
do_load_config([]) ->
    ok.

%%====================================================================
%% @private Internal functions
%%====================================================================

-spec get_config_path() -> {'ok', binary() | string()}.
get_config_path() ->
    Config_path = case code:priv_dir(dorayaki) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?CONFIG_FILE]);
                _ ->
                    filename:join([priv, ?CONFIG_FILE])
            end;
        Dir ->
            filename:join(Dir, ?CONFIG_FILE)
    end,
    {ok, Config_path}.

