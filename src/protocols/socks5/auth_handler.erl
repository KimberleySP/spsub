-module(socks5_auth_handler).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2]).

-record(state, {
    methods = [] :: [auth_method()],
    active_auths = #{} :: #{binary() => auth_state()},
    timeout = 5000 :: pos_integer()
}).

init([Config]) ->
    {ok, #state{methods = Config#config.auth_methods}}. 