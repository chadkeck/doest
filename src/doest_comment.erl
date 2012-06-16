 %% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 75 -*-
 %% ex: ts=4 sw=4 et
 -module(doest_comment).

 -export([init/3, rest_init/2, allowed_methods/2, content_types_provided/2,
          content_types_accepted/2,
          resource_exists/2, delete_resource/2, create_resource/2, to_json/2]).

-include_lib("cowboy/include/http.hrl").

%%===================================================================
%% Public API
%%===================================================================
init(_Transport, _Rq, _Opts) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Rq, _Opts) ->
    {ok, Rq, []}.

allowed_methods(Rq, St) ->
    {['GET', 'PUT', 'DELETE'], Rq, St}.

content_types_accepted(Rq, St) ->
    {[{'*', create_resource}], Rq, St}.

content_types_provided(Rq, St) ->
    {[{{<<"text">>, <<"json">>, []}, to_json}], Rq, St}.

resource_exists(Rq, St) ->
    {false, Rq, St}.

delete_resource(Rq, St) ->
    {true, Rq, St}.

create_resource(Rq, St) ->
    {true, Rq, St}.

to_json(Rq, St) ->
    {<<>>, Rq, St}.
