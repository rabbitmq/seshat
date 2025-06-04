%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2025 Broadcom. All Rights Reserved. The term Broadcom refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(seshat_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include("src/seshat.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [overview,
     counters_with_persistent_term_field_spec,
     format_group,
     format_with_many_labels,
     format_ratio,
     format_time_metrics,
     format_selected_metrics,
     format_with_a_filter,
     prom_format_metrics,
     invalid_fields].

init_per_suite(Config) ->
    logger:remove_handler(default),
    {ok, _} = application:ensure_all_started(seshat),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(seshat),
    ok.

init_per_testcase(TestCaseName, Config) ->
    Group = TestCaseName,
    seshat:new_group(Group),
    [{group, Group} | Config].

end_per_testcase(TestCaseName, _Config) ->
    seshat:delete_group(TestCaseName),
    ok.

%% Test cases
overview(_Config) ->
    Group = ?FUNCTION_NAME,
    Counters = [
                {carrots_eaten_total, 1, counter, "Total number of carrots eaten on a meal"},
                {holes_dug_total, 2, counter, "Total number of holes dug in an afternoon"}
               ],
    seshat:new(Group, "rabbit", Counters),
    set_value(Group, "rabbit", carrots_eaten_total, 3),
    set_value(Group, "rabbit", holes_dug_total, 1),
    Overview = seshat:overview(Group),
    ?assertEqual(
       #{"rabbit" => #{carrots_eaten_total => 3, holes_dug_total => 1}},
       Overview),
    ?assertMatch(#{carrots_eaten_total := 3, holes_dug_total := 1},
                 seshat:overview(Group, "rabbit")),
    ?assertMatch(#{holes_dug_total := 1},
                 seshat:counters(Group, "rabbit", [holes_dug_total])),
    ok.

counters_with_persistent_term_field_spec(_Config) ->
    Group = ?FUNCTION_NAME,
    Counters = [
                {carrots_eaten_total, 1, counter, "Total number of carrots eaten on a meal"},
                {holes_dug_total, 2, counter, "Total number of holes dug in an afternoon"}
               ],
    persistent_term:put(pets_field_spec, Counters),
    seshat:new(Group, "rabbit", {persistent_term, pets_field_spec}),
    set_value(Group, "rabbit", carrots_eaten_total, 3),
    set_value(Group, "rabbit", holes_dug_total, 1),
    Overview = seshat:overview(Group),
    ?assertEqual(
       #{"rabbit" => #{carrots_eaten_total => 3, holes_dug_total => 1}},
       Overview),
    ?assertMatch(#{carrots_eaten_total := 3, holes_dug_total := 1},
                 seshat:overview(Group, "rabbit")),
    ?assertMatch(#{holes_dug_total := 1},
                 seshat:counters(Group, "rabbit", [holes_dug_total])),
    persistent_term:erase(pets_field_spec),
    ok.

format_group(_Config) ->
    Group = ?FUNCTION_NAME,
    Counters = [{reads, 1, counter, "Total reads"}],
    seshat:new(Group, widget1, Counters, #{component => widget1}),
    seshat:new(Group, widget2, Counters, #{component => widget2}),
    seshat:new(Group, screw, Counters), % no labels, will be omitted
    Result = seshat:format(Group),
    ExpectedMap = #{<<"reads">> => #{type => counter,
                               help => "Total reads",
                               values => #{#{component => widget1} => 0.0,
                                           #{component => widget2} => 0.0}}},
    ?assertEqual(ExpectedMap, Result),
    ok.

format_with_many_labels(_Config) ->
    Group = ?FUNCTION_NAME,
    Counters = [{reads, 1, counter, "Total reads"}],
    seshat:new(Group, widget1, Counters, #{component => "widget1", status => up}),
    seshat:new(Group, widget2, Counters, #{component => "widget2", status => down}),
    set_value(Group, widget1, reads, 1),
    set_value(Group, widget2, reads, 2),
    Result = seshat:format(Group),
    ExpectedMap = #{<<"reads">> => #{type => counter,
                               help => "Total reads",
                               values => #{#{component => "widget1", status => up} => 1.0,
                                           #{component => "widget2", status => down} => 2.0}}},
    ?assertEqual(ExpectedMap, Result),
    ok.

format_selected_metrics(_Config) ->
    Group = ?FUNCTION_NAME,
    Counters = [
                {reads, 1, counter, "Total reads"},
                {writes, 2, counter, "Total writes"},
                {lookups, 3, counter, "Total lookups"}
               ],
    seshat:new(Group, thing1, Counters, #{component => "thing1"}),
    seshat:new(Group, thing2, Counters, #{component => "thing2"}),
    Result = seshat:format(Group, #{metrics => [reads, writes]}),
    ExpectedMap = #{<<"reads">> => #{type => counter,
                               help => "Total reads",
                               values => #{#{component => "thing1"} => 0.0,
                                           #{component => "thing2"} => 0.0}},
                    <<"writes">> => #{type => counter,
                                help => "Total writes",
                                values => #{#{component => "thing1"} => 0.0,
                                            #{component => "thing2"} => 0.0}}},
    ?assertEqual(ExpectedMap, Result),
    ok.

format_with_a_filter(_Config) ->
    Group = ?FUNCTION_NAME,
    Counters = [
                {reads, 1, counter, "Total reads"},
                {writes, 2, counter, "Total writes"},
                {lookups, 3, counter, "Total lookups"}
               ],
    seshat:new(Group, thing1, Counters, #{component => "thing1"}),
    seshat:new(Group, thing2, Counters, #{component => "thing2"}),
    FilterFun = fun(#{component := "thing1"}) -> true;
                   (_)                        -> false
                end,
    Result = seshat:format(Group, #{metrics => [reads, writes], filter_fun => FilterFun}),
    ExpectedMap = #{<<"reads">> => #{type => counter,
                                     help => "Total reads",
                                     values => #{#{component => "thing1"} => 0.0}},
                    <<"writes">> => #{type => counter,
                                      help => "Total writes",
                                      values => #{#{component => "thing1"} => 0.0}}},
    ?assertEqual(ExpectedMap, Result),
    ok.

invalid_fields(_Config) ->
    Group = ?FUNCTION_NAME,
    Fields = [{reads, 1, counter, "Total reads"},
              {writes, 3, counter, "Total writes"}], % Index 2 is missing
    ?assertError(invalid_field_specification,
                 seshat:new(Group, invalid_fields, Fields)),
    ok.

format_ratio(_Config) ->
    Group = ?FUNCTION_NAME,
    Counters = [{pings, 1, {gauge, ratio}, "Some ratio that happens to be 0%"},
                {pongs, 2, {gauge, ratio}, "Some ratio that happens to be 17%"},
                {pangs, 3, {gauge, ratio}, "Some ratio that happens to be 33%"},
                {rings, 4, {gauge, ratio}, "Some ratio that happens to be 100%"}],
    seshat:new(Group, test_component, Counters, #{component => test}),
    set_value(Group, test_component, pings, 0),
    set_value(Group, test_component, pongs, 17),
    set_value(Group, test_component, pangs, 33),
    set_value(Group, test_component, rings, 100),
    Result = seshat:format(Group),
    ExpectedMap = #{<<"pings_ratio">> => #{type => gauge,
                               help => "Some ratio that happens to be 0%",
                               values => #{#{component => test} => 0.0}},
                    <<"pongs_ratio">> => #{type => gauge,
                               help => "Some ratio that happens to be 17%",
                               values => #{#{component => test} => 0.17}},
                    <<"pangs_ratio">> => #{type => gauge,
                               help => "Some ratio that happens to be 33%",
                               values => #{#{component => test} => 0.33}},
                    <<"rings_ratio">>  => #{type => gauge,
                               help => "Some ratio that happens to be 100%",
                               values => #{#{component => test} => 1.0}}},
    ?assertEqual(ExpectedMap, Result),
    ok.

format_time_metrics(_Config) ->
    Group = ?FUNCTION_NAME,
    Counters = [
        {job_duration, 1, {counter, time_s}, "Job duration"},
        {short_latency, 2, {gauge, time_ms}, "Short latency"},
        {long_latency, 3, {gauge, time_ms}, "Request latency"}
    ],
    Labels = #{component => test},
    seshat:new(Group, test_component, Counters, Labels),
    set_value(Group, test_component, job_duration, 30),
    set_value(Group, test_component, short_latency, 5),
    set_value(Group, test_component, long_latency, 1500),
    MapResult = seshat:format(Group),
    ExpectedMap = #{
        <<"job_duration_seconds">> => #{type => counter, help => "Job duration", values => #{Labels => 30.0}},
        <<"short_latency_seconds">> => #{type => gauge, help => "Short latency", values => #{Labels => 0.005}},
        <<"long_latency_seconds">> => #{type => gauge, help => "Request latency", values => #{Labels => 1.5}}
    },
    ?assertEqual(ExpectedMap, MapResult),
    Prefix = "myapp",
    MetricNames = [job_duration, short_latency, long_latency],
    TextResult = seshat:prom_format(Group, Prefix, MetricNames),
    ExpectedLines = [
        "# HELP myapp_job_duration_seconds Job duration",
        "# TYPE myapp_job_duration_seconds counter",
        "myapp_job_duration_seconds{component=\"test\"} 30.0",
        "# HELP myapp_short_latency_seconds Short latency",
        "# TYPE myapp_short_latency_seconds gauge",
        "myapp_short_latency_seconds{component=\"test\"} 0.005",
        "# HELP myapp_long_latency_seconds Request latency",
        "# TYPE myapp_long_latency_seconds gauge",
        "myapp_long_latency_seconds{component=\"test\"} 1.5"
    ],
    ExpectedResult = list_to_binary(string:join(ExpectedLines, "\n") ++ "\n"),

    assertEqualIgnoringOrder(ExpectedResult, TextResult),
    ok.

prom_format_metrics(_Config) ->
    Group = ?FUNCTION_NAME,
    Counters = [
                {reads, 1, counter, "Total reads"},
                {writes, 2, counter, "Total writes"},
                {cached, 3, {gauge, ratio}, "Ratio of things served from cache"},
                {latency, 4, {gauge, time_ms}, "Latency"},
                {duration, 5, {gauge, time_s}, "Duration"},
                {npc, 6, gauge, "A metric we don't request in a call to prom_format/3"}
               ],
    seshat:new(Group, thing1, Counters, #{component => "thing1", version => "1.2.3"}),
    seshat:new(Group, thing2, Counters, #{component => "thing2", some_atom => atom_value}),
    seshat:new(Group, thing3, Counters, #{component => "thing3", some_binary => <<"binary_value">>}),
    set_value(Group, thing1, reads, 1),
    set_value(Group, thing1, writes, 2),
    set_value(Group, thing1, cached, 10),
    set_value(Group, thing1, latency, 5),
    set_value(Group, thing1, duration, 123),
    set_value(Group, thing1, npc, 1),  % to be ignored by prom_format/3
    set_value(Group, thing2, reads, 3),
    set_value(Group, thing2, writes, 4),
    set_value(Group, thing2, cached, 100),
    set_value(Group, thing2, latency, 6),
    set_value(Group, thing2, duration, 234),
    set_value(Group, thing2, npc, 1),  % to be ignored by prom_format/3
    set_value(Group, thing3, reads, 1234),
    set_value(Group, thing3, writes, 4321),
    set_value(Group, thing3, cached, 17),
    set_value(Group, thing3, latency, 7),
    set_value(Group, thing3, duration, 345),
    set_value(Group, thing3, npc, 1),  % to be ignored by prom_format/3

    FilteredResult = seshat:prom_format(Group, "acme", [reads, writes, cached, latency, duration]),
    ExpectedFilteredLines = [
        "# HELP acme_reads Total reads",
        "# TYPE acme_reads counter",
        "acme_reads{version=\"1.2.3\",component=\"thing1\"} 1.0",
        "acme_reads{component=\"thing2\",some_atom=\"atom_value\"} 3.0",
        "acme_reads{component=\"thing3\",some_binary=\"binary_value\"} 1234.0",
        "# HELP acme_writes Total writes",
        "# TYPE acme_writes counter",
        "acme_writes{version=\"1.2.3\",component=\"thing1\"} 2.0",
        "acme_writes{component=\"thing2\",some_atom=\"atom_value\"} 4.0",
        "acme_writes{component=\"thing3\",some_binary=\"binary_value\"} 4321.0",
        "# HELP acme_cached_ratio Ratio of things served from cache",
        "# TYPE acme_cached_ratio gauge",
        "acme_cached_ratio{version=\"1.2.3\",component=\"thing1\"} 0.1",
        "acme_cached_ratio{component=\"thing2\",some_atom=\"atom_value\"} 1.0",
        "acme_cached_ratio{component=\"thing3\",some_binary=\"binary_value\"} 0.17",
        "# HELP acme_latency_seconds Latency",
        "# TYPE acme_latency_seconds gauge",
        "acme_latency_seconds{version=\"1.2.3\",component=\"thing1\"} 0.005",
        "acme_latency_seconds{component=\"thing2\",some_atom=\"atom_value\"} 0.006",
        "acme_latency_seconds{component=\"thing3\",some_binary=\"binary_value\"} 0.007",
        "# HELP acme_duration_seconds Duration",
        "# TYPE acme_duration_seconds gauge",
        "acme_duration_seconds{version=\"1.2.3\",component=\"thing1\"} 123.0",
        "acme_duration_seconds{component=\"thing2\",some_atom=\"atom_value\"} 234.0",
        "acme_duration_seconds{component=\"thing3\",some_binary=\"binary_value\"} 345.0"
    ],
    ExpectedFilteredResult = list_to_binary(string:join(ExpectedFilteredLines, "\n") ++ "\n"),

    assertEqualIgnoringOrder(ExpectedFilteredResult, FilteredResult),

    %% make sure that `npc` metric is returned by prom_format/2
    UnfilteredResult = seshat:prom_format(Group, "acme"),
    ExpectedUnfilteredLines = ExpectedFilteredLines ++ [
        "# HELP acme_npc A metric we don't request in a call to prom_format/3",
        "# TYPE acme_npc gauge",
        "acme_npc{component=\"thing2\",some_atom=\"atom_value\"} 1.0",
        "acme_npc{component=\"thing3\",some_binary=\"binary_value\"} 1.0",
        "acme_npc{version=\"1.2.3\",component=\"thing1\"} 1.0"
    ],
    ExpectedUnfilteredResult = list_to_binary(string:join(ExpectedUnfilteredLines, "\n") ++ "\n"),

    assertEqualIgnoringOrder(ExpectedUnfilteredResult, UnfilteredResult),

    ok.

%% Helpers
set_value(Group, Id, Name, Value) ->
    Table = seshat_counters_server:get_table(Group),
    [#entry{cref = CRef, field_spec = FieldSpec}] = ets:lookup(Table, Id),
    Fields = seshat:resolve_fields_spec(FieldSpec),
    {Name, Index, _Type, _Help} = lists:keyfind(Name, 1, Fields),
    ok = counters:put(CRef, Index, Value).

assertEqualIgnoringOrder(Expected, Actual) ->
    ExpectedSorted = lists:sort(binary:split(Expected, <<"\n">>, [global, trim])),
    ActualSorted = lists:sort(binary:split(Actual, <<"\n">>, [global, trim])),
    ?assertEqual(ExpectedSorted, ActualSorted).
