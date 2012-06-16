%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
-module(doest_sup).

-behaviour(supervisor).

-export([start_link/0, reconfigure_cowboy/0]).

-export([init/1]).

%%===================================================================
%% API functions
%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

reconfigure_cowboy() ->
    supervisor:restart_child(?MODULE, bksw_cowboy_sup).

%%===================================================================
%% Supervisor callbacks
%%===================================================================

init(_Args) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,

    CowboySup = {doest_cowboy_sup, {doest_cowboy_sup, start_link, []},
                 Restart, Shutdown, supervisor, [doest_cowboy_sup]},

    {ok, {SupFlags, [CowboySup]}}.
