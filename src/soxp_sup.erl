
-module(soxp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macros for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Args, Type), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(soxp_listener, [{new_socket_handler, fun connection_handler/1}], worker),
                                  ?CHILD(soxp_connection_sup, supervisor)]} }.

connection_handler(Sock) ->
    %% @todo the connection (and hence the soxp_connection) could
    %% close before we can message it, causing this function to throw.

    %% spin up a new soxp_connection (under the control of the
    %% soxp_connection_sup)
    {ok, Pid} = soxp_connection_sup:start_child(Sock),

    %% socket handover
    gen_tcp:controlling_process(Sock, Pid),

    %% notify soxp_connection that socket handover complete
    Pid ! socket_handed_over.
