-module(soxp_connection).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(soxp_connection_state, {sock}).

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
    %% unregistered
    gen_server:start_link(?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Sock) when is_port(Sock) ->
    %% store socket; wait for handover message
    {ok, #soxp_connection_state{sock=Sock}}.

handle_call(_Request, _From, State) ->
    %% ignore
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    %% ignore
    {noreply, State}.

handle_info(socket_handed_over, State) ->
    %% socket handover complete; start comms
    inet:setopts(State#soxp_connection_state.sock, [{active, true}]),
    gen_tcp:send(State#soxp_connection_state.sock, <<"Hello; echo active\n">>),
    {noreply, State};
handle_info({tcp, _, Data}, State) ->
    %% echo
    gen_tcp:send(State#soxp_connection_state.sock, Data),
    {noreply, State};
handle_info({tcp_closed, _}, State) ->
    %% connection closed; exit
    {stop, normal, State};
handle_info(_Info, State) ->
    %% ignore
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#soxp_connection_state.sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

