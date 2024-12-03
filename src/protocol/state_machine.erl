-module(protocol_state_machine).
-behaviour(gen_statem).

%% API
-export([start_link/1, stop/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([handshake/3, authenticating/3, negotiating/3, 
         streaming/3, error/3]).

-record(data, {
    socket :: port(),
    config :: map(),
    buffer :: binary(),
    metrics :: #{atom() => integer()},
    auth_attempts = 0 :: non_neg_integer(),
    max_auth_attempts = 3 :: pos_integer(),
    timeout = 5000 :: pos_integer(),
    protocol_version :: binary(),
    extensions = [] :: [atom()],
    compression_level = 0 :: 0..9,
    encryption_enabled = false :: boolean(),
    tls_options :: map()
}).

%% 复杂状态机实现
init([Config]) ->
    process_flag(trap_exit, true),
    {ok, handshake, #data{config = Config}}.

callback_mode() -> 
    [state_functions, state_enter].

%% 状态转换处理
handshake(enter, _OldState, Data) ->
    schedule_timeout(Data#data.timeout),
    {keep_state, Data};
handshake(info, {tcp, Socket, Packet}, Data) ->
    case parse_handshake(Packet) of
        {ok, Version, Extensions} ->
            NewData = Data#data{
                socket = Socket,
                protocol_version = Version,
                extensions = Extensions
            },
            {next_state, authenticating, NewData};
        {error, Reason} ->
            {next_state, error, Data#data{
                buffer = <<>>,
                metrics = maps:update_with(
                    error_count,
                    fun(X) -> X + 1 end,
                    1,
                    Data#data.metrics
                )
            }}
    end;
...(about 200 more lines of state machine logic)...

%% 高级协议协商
negotiate_protocol(Socket, Version, Extensions) ->
    AvailableExtensions = [compression, encryption, multiplexing],
    CommonExtensions = lists:filter(
        fun(Ext) -> 
            lists:member(Ext, Extensions) 
        end,
        AvailableExtensions
    ),
    case negotiate_parameters(CommonExtensions) of
        {ok, Params} ->
            setup_protocol_handlers(Socket, Params);
        {error, Reason} ->
            {error, {protocol_negotiation_failed, Reason}}
    end.

%% 高级错误恢复
handle_error(State, Reason, Data) ->
    error_logger:error_msg(
        "Error in state ~p: ~p~n",
        [State, Reason]
    ),
    case should_retry(State, Data#data.auth_attempts) of
        true ->
            retry_operation(State, Data);
        false ->
            {stop, normal, Data}
    end. 