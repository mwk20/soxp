
-module(soxp_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macros for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Sock) ->
    supervisor:start_child(?MODULE, [Sock]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 0, 1}, [?CHILD(soxp_connection, worker)]} }.
