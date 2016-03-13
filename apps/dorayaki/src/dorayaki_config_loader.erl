%%%-------------------------------------------------------------------
%% @doc dorayaki config_loader public API
%% @end
%%%-------------------------------------------------------------------

-module(dorayaki_config_loader).

-ifdef(TEST).

-define(CONFIG_FILE, "dorayaki_test.cfg").

-else.

-define(CONFIG_FILE, "dorayaki.cfg").

-endif.

-define(APPLICATION, dorayaki).

-export([get_env/1]).

-export([load_config/0]).

get_env(Key) ->
    {ok, Value} = application:get_env(?APPLICATION, Key),
	Value.

%%====================================================================
%% Internal functions
%%====================================================================

get_config_path() ->
    Config_path = case code:priv_dir(?APPLICATION) of
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

load_config() ->
    {ok, Config_path} = get_config_path(),
    lager:log(debug, "console", "Config path: ~p", [Config_path]),
    {ok, ConfigList} = file:consult(Config_path),
    lager:log(debug, "console", "ConfigList: ~p", [ConfigList]),
    load_config(ConfigList).

load_config([{ConfigName, ConfigValue}|RestConfig]) when ConfigName =:= log -> 
    Log_level = proplists:get_value(level, ConfigValue),
    lager:set_loglevel(lager_console_backend, Log_level),
    lager:set_loglevel(lager_file_backend, "console.log", Log_level),
    load_config(RestConfig);

load_config([{ConfigName, ConfigValue}|RestConfig]) -> 
    lager:log(debug, "console", "ConfigName: ~p", [ConfigName]),
    lager:log(debug, "console", "ConfigValue: ~p", [ConfigValue]),
    application:set_env(?APPLICATION, ConfigName, ConfigValue),
    load_config(RestConfig);

load_config([]) ->
    {ok, done}.

