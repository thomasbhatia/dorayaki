%%%-------------------------------------------------------------------
%% @doc dorayaki config_loader public API
%% @end
%%%-------------------------------------------------------------------

-define(CONFIG_FILE, "dorayaki.cfg").

-module(config_loader).

-define(APPLICATION, dorayaki).

-define(LOG_LEVEL, config_loader:get_env(log_level)).

-export([get_env/1]).

get_env(Key) ->
	case application:get_env(?APPLICATION, Key) of   
		{ok, Value} -> 
			io:format("found ~p~n", [Value]);
			% Value;
		_ ->
			load_config(),
			Value = application:get_env(?APPLICATION, Key),
			get_env(Key)
	end,
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
    io:format("Config path: ~p~n", [Config_path]),
    lager:info("~s is ~s!", [lager, cool]),
    lager:warning("but pay ~s!", [attention]),
    lager:error("there is always some ~s", [error]),
    case file:consult(Config_path) of 
        {ok, Config} ->
            io:format("#############################~n"),
            io:format("Reading configuration file...~n"),

            io:format("Config File: ~p~n", [Config]),
            % Client config
            [{client, CLIENT_CONFIG}] = [L || {client, _}=L <- Config],
            io:format("Client config: ~p~n", [CLIENT_CONFIG]),

            [{port, CLIENT_PORT}] = [L || {port, _}=L <- CLIENT_CONFIG],
            set_env(client_port, CLIENT_PORT),
            io:format("Client Port: ~p~n", [CLIENT_PORT]),

            % Host config
            [{host, HOST_CONFIG}] = [L || {host, _}=L <- Config],
            io:format("Host config: ~p~n", [HOST_CONFIG]),

            [{ip, HOST_IP}] = [L || {ip, _}=L <- HOST_CONFIG],
            set_env(host_ip, HOST_IP),
            io:format("Host IP: ~p~n", [HOST_IP]),

            [{port, HOST_PORT}] = [L || {port, _}=L <- HOST_CONFIG],
            set_env(host_port, HOST_PORT),
            io:format("Host Port: ~p~n", [HOST_PORT]),

            % Search config
            [{search_header, SearchHeader}] = [L || {search_header, _}=L <- Config],
            set_env(search_header, SearchHeader),
            io:format("Search header: ~p~n", [SearchHeader]),

            [{search_avps, SearchAVPs}] = [L || {search_avps, _}=L <- Config],
            set_env(search_avps, SearchAVPs),
            io:format("Search AVPs: ~p~n", [SearchAVPs]),

            [{replace_avp, ReplaceAVP}] = [L || {replace_avp, _}=L <- Config],
            set_env(replace_avp, ReplaceAVP),
            io:format("Replace AVPs: ~p~n", [ReplaceAVP]),

            % Log level
            [{log_level, Log_level}] = [L || {replace_avp, _}=L <- Config],
            set_env(log_level, Log_level),
            io:format("log_level: ~p~n", [Log_level]),

            % Unknown = [L || _=L <- Config],
            % exit("Unknown client config: ~p~n", [Unknown]),

            io:format("Finished reading configuration file.~n"),
            io:format("#############################~n");

        {error, Why} -> 
            % io:format("Couldn't find config file")
            % {ok, Dir} = file:get_cwd(),
            % io:format("Dir: ~p~n", [Dir]),
            % io:format("Config file: ~p~n", [?CONFIG_FILE]),
            io:format("Error caused by: ~p~n", [Why]),
            exit("Error reading file")
    end.

% % Parse config
% parse_config([]) ->
%     ParsedList = [],
%     parse_config(Config, ParsedList, []).


% parse_config({client, CLIENT_CONFIG}) ->
%     parse_client_config([{client, CLIENT_CONFIG}]);

% parse_config([{host, HOST_CONFIG}]) ->
%     parse_host_config([{host, HOST_CONFIG}]);

% parse_config(_) ->
%     exit("Error reading config file").

% % Parse client config
% parse_client_config([{port, CLIENT_PORT}]) ->
%     set_env(client_port, CLIENT_PORT),
%     io:format("Client Port: ~p~n", [CLIENT_PORT]);

% parse_client_config(_) ->
%     exit("Unknown client config.").

% % Parse host config
% parse_host_config([{ip, HOST_IP}]) ->
%     set_env(host_ip, HOST_IP),
%     io:format("Host IP: ~p~n", [HOST_IP]);

% parse_host_config([{port, HOST_PORT}]) ->
%     set_env(host_ip, HOST_PORT),
%     io:format("Host IP: ~p~n", [HOST_PORT]);

% parse_host_config(_) ->
%     exit("Unknown host config").