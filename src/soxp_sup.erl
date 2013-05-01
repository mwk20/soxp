
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
    {ok, { {one_for_one, 5, 10}, [?CHILD(soxp_listener, [{new_socket_handler, fun example_handler/1}], worker),
                                  ?CHILD(soxp_connection_sup, supervisor)]} }.

example_handler(Sock) ->
    erlang:spawn(fun() ->
                         gen_tcp:send(Sock, <<"Foo">>),
                         timer:sleep(750),
                         gen_tcp:send(Sock, <<"Bar">>),
                         timer:sleep(750),
                         gen_tcp:send(Sock, <<"Baz">>),
                         gen_tcp:close(Sock)
                 end).
