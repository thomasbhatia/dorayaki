%%%-------------------------------------------------------------------
%% @doc dorayaki config_loader public API
%% @end
%%%-------------------------------------------------------------------

-define(CONFIG_FILE, "dorayaki.cfg").

-module(config_loader).

-define(APPLICATION, dorayaki).

-export([load_config/0]).
-export([get_env/1]).

get_env(Key) ->
    {ok, Value} = application:get_env(?APPLICATION, Key),
	Value.

%%====================================================================
%% Internal functions
%%====================================================================

set_env(Key, Value) ->
    application:set_env(?APPLICATION, Key, Value).

load_config() ->
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
    parse_config(Config_path).


parse_config(Config_path) ->
    lager:log(debug, "console", "there is always some ~s", [doioio]),

    case file:consult(Config_path) of 
        {ok, Config} ->
            lager:log(info, "console", "#############################"),
            lager:log(info, "console", "Reading configuration file..."),
            lager:log(info, "console", "Config File: ~p", [Config]),

            % Client config
            [{client, Client_config}] = [L || {client, _}=L <- Config],
            lager:log(info, "console", "Client config: ~p", [Client_config]),

            [{port, Client_port}] = [L || {port, _}=L <- Client_config],
            set_env(client_port, Client_port),
            lager:log(info, "console", "Client Port: ~p", [Client_port]),

            % Host config
            [{host, Host_config}] = [L || {host, _}=L <- Config],
            lager:log(info, "console", "Host config: ~p", [Host_config]),

            [{ip, Host_IP}] = [L || {ip, _}=L <- Host_config],
            set_env(host_ip, Host_IP),
            lager:log(info, "console", "Host IP: ~p", [Host_IP]),

            [{port, Host_port}] = [L || {port, _}=L <- Host_config],
            set_env(host_port, Host_port),
            lager:log(info, "console", "Host Port: ~p", [Host_port]),

            % Search config
            [{search_header, Search_header}] = [L || {search_header, _}=L <- Config],
            set_env(search_header, Search_header),
            lager:log(info, "console", "Search header: ~p", [Search_header]),

            [{search_avps, Search_AVPs}] = [L || {search_avps, _}=L <- Config],
            set_env(search_avps, Search_AVPs),
            lager:log(info, "console", "Search AVPs: ~p", [Search_AVPs]),

            [{replace_avp, Replace_AVP}] = [L || {replace_avp, _}=L <- Config],
            set_env(replace_avp, Replace_AVP),
            lager:log(info, "console", "Replace AVP: ~p", [Replace_AVP]),

            % Log level
            [{log_level, Log_level}] = [L || {log_level, _}=L <- Config],
            set_env(log_level, Log_level),
            lager:log(info, "console", "log_level: ~p", [Log_level]),

            lager:set_loglevel(lager_console_backend, Log_level),
            lager:set_loglevel(lager_file_backend, "console.log", Log_level),

            lager:log(info, "console", "Finished reading configuration file."),
            lager:log(info, "console", "#############################");

        {error, Why} -> 
            lager:log(error, "console", "Error caused by: ", [Why]),
            exit("Error reading file")
    end.
