%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2023 Broadcom. All Rights Reserved. The term Broadcom refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(seshat_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(seshat).

cleanup(_) ->
    ok = application:stop(seshat).

test_suite_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [ fun overview/0,
       fun counters_with_persistent_term_field_spec/0,
       fun prometheus_format_multiple_names/0,
       fun prometheus_format_single_name/0,
       fun prometheus_format_with_labels/0,
       fun invalid_fields/0 ]}.

overview() ->
    Group = ?FUNCTION_NAME,
    Counters = [
                {
                 carrots_eaten_total, 1, counter,
                 "Total number of carrots eaten on a meal"
                },
                {
                 holes_dug_total, 2, counter,
                 "Total number of holes dug in an afternoon"
                }
               ],
    seshat:new_group(Group),
    seshat:new(Group, "rabbit", Counters),
    Ref = seshat:fetch(Group, "rabbit"),
    counters:add(Ref, 1, 3),
    counters:add(Ref, 2, 1),
    Overview = seshat:overview(Group),
    ?assertEqual(
       #{"rabbit" => #{carrots_eaten_total => 3,
                       holes_dug_total => 1}},
       Overview),

    ?assertMatch(#{carrots_eaten_total := 3,
                   holes_dug_total := 1},
                 seshat:overview(Group, "rabbit")),

    ?assertMatch(#{holes_dug_total := 1},
                 seshat:counters(Group, "rabbit", [holes_dug_total])),
    ok.

counters_with_persistent_term_field_spec() ->
    Group = ?FUNCTION_NAME,
    Counters = [
                {
                 carrots_eaten_total, 1, counter,
                 "Total number of carrots eaten on a meal"
                },
                {
                 holes_dug_total, 2, counter,
                 "Total number of holes dug in an afternoon"
                }
               ],

    persistent_term:put(pets_field_spec, Counters),
    seshat:new_group(Group),
    seshat:new(Group, "rabbit", {persistent_term, pets_field_spec}),
    Ref = seshat:fetch(Group, "rabbit"),
    counters:add(Ref, 1, 3),
    counters:add(Ref, 2, 1),
    Overview = seshat:overview(Group),
    ?assertEqual(
       #{"rabbit" => #{carrots_eaten_total => 3,
                       holes_dug_total => 1}},
       Overview),

    ?assertMatch(#{carrots_eaten_total := 3,
                   holes_dug_total := 1},
                 seshat:overview(Group, "rabbit")),

    ?assertMatch(#{holes_dug_total := 1},
                 seshat:counters(Group, "rabbit", [holes_dug_total])),

    ok.

prometheus_format_multiple_names() ->
    Group = people,
    Counters = [{foo, 1, counter, "Total foos given"}],
    seshat:new_group(Group),
    seshat:new(Group, {name, you}, Counters),
    seshat:new(Group, {name, me}, Counters),
    PrometheusFormat = seshat:format(Group),
    ExpectedPrometheusFormat = #{foo => #{type => counter,
                                          help => "Total foos given",
                                          values => #{{name, me} => 0,
                                                      {name, you} => 0}}},
    ?assertEqual(ExpectedPrometheusFormat, PrometheusFormat),
    ok.

prometheus_format_single_name() ->
    Group = people,
    Counters = [{foo, 1, counter, "Total foos given"}],
    seshat:new_group(Group),
    seshat:new(Group, {name, you}, Counters),
    seshat:new(Group, {name, me}, Counters),
    PrometheusFormat = seshat:format(Group, {name, me}),
    ExpectedPrometheusFormat = #{foo => #{type => counter,
                                          help => "Total foos given",
                                          values => #{{name, me} => 0}}},
    ?assertEqual(ExpectedPrometheusFormat, PrometheusFormat),
    ok.

prometheus_format_with_labels() ->
    Group = people,
    Counters = [{foo, 1, counter, "Total foos given"}],
    seshat:new_group(Group),
    seshat:new(Group, {name, you}, Counters, #{name => "Monet"}),
    seshat:new(Group, {name, me}, Counters, #{name => "Manet"}),
    PrometheusFormat = seshat:format(Group),
    ExpectedPrometheusFormat = #{foo => #{type => counter,
                                          help => "Total foos given",
                                          values => #{#{name => "Monet"} => 0,
                                                      #{name => "Manet"} => 0}}},
    ?assertEqual(ExpectedPrometheusFormat, PrometheusFormat),
    ok.

invalid_fields() ->
    Group = people,
    Fields = [{foo, 1, counter, "Total foos given"},
              {boo, 3, counter, "Total boos given"}],
    seshat:new_group(Group),
    ?assertError(invalid_field_specification,
                 seshat:new(Group, invalid_fields, Fields)),

    ok.

