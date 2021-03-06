%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 75 -*-
%% ex: ts=4 sw=4 et
-module(doest_app).

-export([init/3, rest_init/2, allowed_methods/2, content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2, delete_resource/2, create_resource/2, to_xml/2]).

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
    {[{{<<"text">>, <<"xml">>, []}, to_xml}], Rq, St}.

resource_exists(#http_req{host = [Bucket | _]} = Rq, St) ->
    {bookshelf_store:bucket_exists(Bucket), Rq, St}.

delete_resource(#http_req{host = [Bucket | _]} = Rq, St) ->
    case bookshelf_store:bucket_delete(Bucket) of
        ok ->
            {true, Rq, St};
        _ ->
            {false, Rq, St}
    end.

create_resource(#http_req{host = [Bucket | _]} = Rq, St) ->
    case bookshelf_store:bucket_create(Bucket) of
      ok -> {true, Rq, St};
      _ -> {false, Rq, St}
    end.

to_xml(#http_req{host = [Bucket | _]} = Rq, St) ->
    Objects = bookshelf_store:obj_list(Bucket),
    Term = bksw_xml:list_objects(Bucket, Objects),
    Body = bksw_xml:write(Term),
    {Body, Rq, St}.

%%===================================================================
%% Eunit Tests
%%===================================================================
-ifndef(NO_TESTS).

-include_lib("eunit/include/eunit.hrl").

allowed_methods_test_() ->
    [{"should only support 'GET', 'PUT' and 'DELETE'",
      fun () ->
              Expected = ['GET', 'PUT', 'DELETE'],
              {Allowed, _, _} = allowed_methods(#http_req{pid=self()}, % make dialyzer happy
                                                undefined),
              ?assertEqual((length(Expected)), (length(Allowed))),
              Result = sets:from_list(lists:merge(Expected, Allowed)),
              ?assertEqual((length(Expected)), (sets:size(Result)))
      end}].

content_types_accepted_test_() ->
    [{"should only support PUT with 'undefined' "
      "(absent) Content-Type",
      fun () ->
              {Types, _, _} = content_types_accepted(#http_req{pid=self()}, % make dialyzer happy
                                                     undefined),
              ?assertEqual(1, length(Types)),
              ?assertEqual(true, lists:keymember('*', 1, Types))
      end}].

content_types_provided_test_() ->
    [{"should only support text/xml output",
      fun () ->
              {Types, _, _} = content_types_provided(#http_req{pid=self()}, % make dialyzer happy
                                                     undefined),
              ?assertEqual(1, length(Types)),
              ?assertEqual(true, lists:keymember({<<"text">>, <<"xml">>, []}, 1,
                                                 Types))
      end}].

-endif.
