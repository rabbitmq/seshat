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
       fun prometheus_format_group/0,
       fun prometheus_format_name_from_group/0,
       fun prometheus_format_with_labels/0,
       fun prometheus_format_ratio/0,
       fun prometheus_foobar/0,
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

prometheus_format_group() ->
    Group = people,
    Counters = [{foo, 1, counter, "Total foos given"}],
    seshat:new_group(Group),
    seshat:new(Group, you, Counters, #{name => you}),
    seshat:new(Group, me, Counters, #{name => me}),
    seshat:new(Group, ghost, Counters), % no labels, will be omitted
    PrometheusFormat = seshat:format(Group),
    ExpectedPrometheusFormat = #{foo => #{type => counter,
                                          help => "Total foos given",
                                          values => #{#{name => me} => 0,
                                                      #{name => you} => 0}}},
    ?assertEqual(ExpectedPrometheusFormat, PrometheusFormat),
    ok.

prometheus_format_name_from_group() ->
    Group = people,
    Counters = [{foo, 1, counter, "Total foos given"}],
    seshat:new_group(Group),
    seshat:new(Group, {name, you}, Counters, #{name => you}),
    seshat:new(Group, {name, me}, Counters, #{name => me}),
    seshat:new(Group, ghost, Counters), % no labels, will be omitted
    PrometheusFormat = seshat:format_one(Group, {name, me}),
    ExpectedPrometheusFormat = #{foo => #{type => counter,
                                          help => "Total foos given",
                                          values => #{#{name => me} => 0}}},
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

prometheus_foobar() ->
    Group = things,
    Counters = [
                {foo, 1, counter, "Total foos"},
                {bar, 2, counter, "Total bars"},
                {baz, 3, counter, "Total bazs"}
               ],
    seshat:new_group(Group),
    seshat:new(Group, thing1, Counters, #{name => "thing1"}),
    seshat:new(Group, thing2, Counters, #{name => "thing2"}),
    PrometheusFormat = seshat:format(Group, [foo, bar]),
    ExpectedPrometheusFormat = #{foo => #{type => counter,
                                          help => "Total foos",
                                          values => #{#{name => "thing1"} => 0,
                                                      #{name => "thing2"} => 0}},
                                 bar => #{type => counter,
                                          help => "Total bars",
                                          values => #{#{name => "thing1"} => 0,
                                                      #{name => "thing2"} => 0}}},
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

prometheus_format_ratio() ->
    Group = ratios,
    Counters = [{zero, 1, ratio, "Some ratio that happens to be 0%"},
                {seventeen, 2, ratio, "Some ratio that happens to be 17%"},
                {third, 3, ratio, "Some ratio that happens to be 33%"},
                {all, 4, ratio, "Some ratio that happens to be 100%"}],
    seshat:new_group(Group),
    seshat:new(Group, {name, test}, Counters, #{name => test}),
    Ref = seshat:fetch(Group, {name, test}),
    counters:put(Ref, 1, 0),
    counters:put(Ref, 2, 17),
    counters:put(Ref, 3, 33),
    counters:put(Ref, 4, 100),

    PrometheusFormat = seshat:format(Group),
    ExpectedPrometheusFormat = #{zero => #{type => gauge,
                                           help => "Some ratio that happens to be 0%",
                                           values => #{#{name => test} => 0.0}},
                                 seventeen => #{type => gauge,
                                            help => "Some ratio that happens to be 17%",
                                            values => #{#{name => test} => 0.17}},
                                 third => #{type => gauge,
                                            help => "Some ratio that happens to be 33%",
                                            values => #{#{name => test} => 0.33}},
                                 all => #{type => gauge,
                                          help => "Some ratio that happens to be 100%",
                                          values => #{#{name => test} => 1.0}}},

    maps:foreach(
      fun (Name, Value) ->
              ?assertEqual(Value, maps:get(Name, PrometheusFormat))
      end,
      ExpectedPrometheusFormat),
    ok.

