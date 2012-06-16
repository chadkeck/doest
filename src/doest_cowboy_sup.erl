%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
-module(doest_cowboy_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%%===================================================================
%% API functions
%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%===================================================================
%% Supervisor callbacks
%%===================================================================

init(_Args) ->
    %% This will reconfigure the system each time we startup.
    {Port, Dir} = get_configuration(),
    Dispatch = {[<<"static">>, '...'], cowboy_http_static,
                [{directory, Dir}]},

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    ListenerSup = cowboy:child_spec(bookshelf_http_listener,
                                    100, cowboy_tcp_transport, [{port, Port}],
                                    cowboy_http_protocol, [{dispatch, Dispatch}]),
    {ok,
     {{RestartStrategy, MaxRestarts,
       MaxSecondsBetweenRestarts},
      [ListenerSup]}}.

get_configuration() ->
    Port = application:get_env(doest, port, 100),
    ServDir = application:get_env(doest, repo, code:priv_dir(doest)),
    {Port, ServDir}.
