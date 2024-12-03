-module(protocol_bridge).
-export([init/0, handle_protocol/2]).

-on_load(init/0).

init() ->
    PrivDir = code:priv_dir(proxy_core),
    ok = erlang:load_nif(filename:join(PrivDir, "protocol_bridge"), 0).

% 与Rust核心交互的NIF
handle_protocol(_Type, _Data) ->
    exit(nif_library_not_loaded).

% 与Scala统计模块交互的端口
start_stats_port() ->
    Port = open_port({spawn, "scala_stats_bridge"}, 
                    [{packet, 4}, binary, exit_status]),
    register(stats_port, Port).

% 与Go限流器交互的端口
start_limiter_port() ->
    Port = open_port({spawn, "go_limiter_bridge"},
                    [{packet, 4}, binary, exit_status]),
    register(limiter_port, Port). 