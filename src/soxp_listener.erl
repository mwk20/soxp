-module(soxp_listener).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(soxp_listener_state, {lsock,
                              new_socket_handler}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{new_socket_handler, Fun}]) when is_function(Fun, 1) ->
%%init(Args) ->
    %% ensure terminate is called by supervisor
    process_flag(trap_exit, true),

    %% create listening socket
    {ok, LSock} = gen_tcp:listen(8100, [binary, {nodelay, true}, {packet, raw}, {reuseaddr, true}, {active, false}]),

    %% we use the gen_server timeout to accept connections
    {ok, #soxp_listener_state{lsock=LSock, new_socket_handler=Fun}, 0}.

handle_call(_Request, _From, State) ->
    %% ignore; timeout required to continue accepting connections
    {reply, ok, State, 0}.

handle_cast(_Msg, State) ->
    %% ignore; timeout required to continue accepting connections
    {noreply, State, 0}.

handle_info(timeout, State) ->
    %% wait for up to 100ms for a new connection
    %% we wait for a finite time in order to keep servicing otp events
    case gen_tcp:accept(State#soxp_listener_state.lsock, 100) of
        {ok, Sock} ->
            %% pass new connections to new_socket_handler
            (State#soxp_listener_state.new_socket_handler)(Sock);
        {error, timeout} ->
            %% no connection within timeout
            ok
    end,
    {noreply, State, 0};
handle_info(_Info, State) ->
    %% ignore; timeout required to continue accepting connections
    {noreply, State, 0}.

terminate(_Reason, State) ->
    gen_tcp:close(State#soxp_listener_state.lsock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

