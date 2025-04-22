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
       fun format_group/0,
       fun format_one/0,
       fun format_with_many_labels/0,
       fun format_ratio/0,
       fun format_time_metrics/0,
       fun format_selected_metrics/0,
       fun text_format_selected_metrics/0,
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
    set_value(Group, "rabbit", carrots_eaten_total, 3),
    set_value(Group, "rabbit", holes_dug_total, 1),
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
    set_value(Group, "rabbit", carrots_eaten_total, 3),
    set_value(Group, "rabbit", holes_dug_total, 1),
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

format_group() ->
    Group = ?FUNCTION_NAME,
    Counters = [{reads, 1, counter, "Total reads"}],
    seshat:new_group(Group),
    seshat:new(Group, widget1, Counters, #{component => widget1}),
    seshat:new(Group, widget2, Counters, #{component => widget2}),
    seshat:new(Group, screw, Counters), % no labels, will be omitted
    Result = seshat:format(Group),
    ExpectedMap = #{reads => #{type => counter,
                               help => "Total reads",
                               values => #{#{component => widget1} => 0,
                                           #{component => widget2} => 0}}},
    ?assertEqual(ExpectedMap, Result),
    ok.

format_one() ->
    Group = ?FUNCTION_NAME,
    Counters = [{reads, 1, counter, "Total reads"}],
    seshat:new_group(Group),
    seshat:new(Group, widget1, Counters, #{component => widget1}),
    seshat:new(Group, widget2, Counters, #{component => widget2}),
    Result = seshat:format_one(Group, widget2),
    ExpectedMap = #{reads => #{type => counter,
                               help => "Total reads",
                               values => #{#{component => widget2} => 0}}},
    ?assertEqual(ExpectedMap, Result),
    ok.

format_with_many_labels() ->
    Group = ?FUNCTION_NAME,
    Counters = [{reads, 1, counter, "Total reads"}],
    seshat:new_group(Group),
    seshat:new(Group, widget1, Counters, #{component => "widget1", status => up}),
    seshat:new(Group, widget2, Counters, #{component => "widget2", status => down}),
    set_value(Group, widget1, reads, 1),
    set_value(Group, widget2, reads, 2),
    Result = seshat:format(Group),
    ExpectedMap = #{reads => #{type => counter,
                               help => "Total reads",
                               values => #{#{component => "widget1", status => up} => 1,
                                           #{component => "widget2", status => down} => 2}}},
    ?assertEqual(ExpectedMap, Result),
    ok.

format_selected_metrics() ->
    Group = ?FUNCTION_NAME,
    Counters = [
                {reads, 1, counter, "Total reads"},
                {writes, 2, counter, "Total writes"},
                {lookups, 3, counter, "Total lookups"}
               ],
    seshat:new_group(Group),
    seshat:new(Group, thing1, Counters, #{component => "thing1"}),
    seshat:new(Group, thing2, Counters, #{component => "thing2"}),
    Result = seshat:format(Group, [reads, writes]),
    ExpectedMap = #{reads => #{type => counter,
                               help => "Total reads",
                               values => #{#{component => "thing1"} => 0,
                                           #{component => "thing2"} => 0}},
                    writes => #{type => counter,
                                help => "Total writes",
                                values => #{#{component => "thing1"} => 0,
                                            #{component => "thing2"} => 0}}},
    ?assertEqual(ExpectedMap, Result),
    ok.

invalid_fields() ->
    Group = widgets,
    Fields = [{reads, 1, counter, "Total reads"},
              {writes, 3, counter, "Total writes"}],
    seshat:new_group(Group),
    ?assertError(invalid_field_specification,
                 seshat:new(Group, invalid_fields, Fields)),

    ok.

format_ratio() ->
    Group = ?FUNCTION_NAME,
    Counters = [{pings, 1, ratio, "Some ratio that happens to be 0%"},
                {pongs, 2, ratio, "Some ratio that happens to be 17%"},
                {pangs, 3, ratio, "Some ratio that happens to be 33%"},
                {rings, 4, ratio, "Some ratio that happens to be 100%"}],
    seshat:new_group(Group),
    seshat:new(Group, test_component, Counters, #{component => test}),
    set_value(Group, test_component, pings, 0),
    set_value(Group, test_component, pongs, 17),
    set_value(Group, test_component, pangs, 33),
    set_value(Group, test_component, rings, 100),

    Result = seshat:format(Group),
    ExpectedMap = #{pings => #{type => gauge,
                               help => "Some ratio that happens to be 0%",
                               values => #{#{component => test} => 0.0}},
                    pongs => #{type => gauge,
                               help => "Some ratio that happens to be 17%",
                               values => #{#{component => test} => 0.17}},
                    pangs => #{type => gauge,
                               help => "Some ratio that happens to be 33%",
                               values => #{#{component => test} => 0.33}},
                    rings  => #{type => gauge,
                               help => "Some ratio that happens to be 100%",
                               values => #{#{component => test} => 1.0}}},
    ?assertEqual(ExpectedMap, Result),
    ok.

    format_time_metrics() ->
        Group = ?FUNCTION_NAME,
        Counters = [
                    {job_duration, 2, time_s, "Job duration"},
                    {short_latency, 3, time_ms, "Short latency"},
                    {long_latency, 1, time_ms, "Request latency"}
                   ],
        seshat:new_group(Group),
        Labels = #{component => test},
        seshat:new(Group, test_component, Counters, Labels),

        % Set values (1500 ms, 30 s, 5 ms)
        set_value(Group, test_component, job_duration, 30),
        set_value(Group, test_component, short_latency, 5),
        set_value(Group, test_component, long_latency, 1500),

        MapResult = seshat:format(Group),
        ExpectedMap = #{
            job_duration => #{type => gauge,
                              help => "Job duration",
                              values => #{Labels => 30.0}},
            short_latency => #{type => gauge,
                               help => "Short latency",
                               values => #{Labels => 0.005}},
            long_latency => #{type => gauge,
                              help => "Request latency",
                              values => #{Labels => 1.5}}
        },
        ?assertEqual(ExpectedMap, MapResult),

        Prefix = "myapp",
        MetricNames = [job_duration, short_latency, long_latency], % Added new metric name
        TextResult = seshat:text_format(Group, Prefix, MetricNames),

        ExpectedLines = [
            "# HELP myapp_job_duration_seconds Job duration",
            "# TYPE myapp_job_duration_seconds gauge",
            "myapp_job_duration_seconds{component=\"test\"} 30.0",
            "# HELP myapp_short_latency_seconds Short latency",
            "# TYPE myapp_short_latency_seconds gauge",
            "myapp_short_latency_seconds{component=\"test\"} 0.005",
            "# HELP myapp_long_latency_seconds Request latency",
            "# TYPE myapp_long_latency_seconds gauge",
            "myapp_long_latency_seconds{component=\"test\"} 1.5"
        ],
        ExpectedResult = list_to_binary(string:join(ExpectedLines, "\n") ++ "\n"),

        ?assertEqual(ExpectedResult, TextResult),
        ok.

text_format_selected_metrics() ->
    Group = widgets,
    Counters = [
                {reads, 1, counter, "Total reads"},
                {writes, 2, counter, "Total writes"},
                {cached, 3, ratio, "Ratio of things served from cache"},
                {latency, 4, time_ms, "Latency"},
                {duration, 5, time_s, "Duration"},
                {npc, 6, gauge, "A metric we don't request in a call to text_format/3"}
               ],
    seshat:new_group(Group),
    seshat:new(Group, thing1, Counters, #{component => "thing1", version => "1.2.3"}),
    seshat:new(Group, thing2, Counters, #{component => "thing2", some_atom => atom_value}),
    seshat:new(Group, thing3, Counters, #{component => "thing3", some_binary => <<"binary_value">>}),
    set_value(Group, thing1, reads, 1),
    set_value(Group, thing1, writes, 2),
    set_value(Group, thing1, cached, 10),
    set_value(Group, thing1, latency, 5),
    set_value(Group, thing1, duration, 123),
    set_value(Group, thing1, npc, 1), % to be ignored
    set_value(Group, thing2, reads, 3),
    set_value(Group, thing2, writes, 4),
    set_value(Group, thing2, cached, 100),
    set_value(Group, thing2, latency, 6),
    set_value(Group, thing2, duration, 234),
    set_value(Group, thing2, npc, 1), % to be ignored
    set_value(Group, thing3, reads, 1234),
    set_value(Group, thing3, writes, 4321),
    set_value(Group, thing3, cached, 17),
    set_value(Group, thing3, latency, 7),
    set_value(Group, thing3, duration, 345),
    set_value(Group, thing3, npc, 1), % to be ignored

    Result = seshat:text_format(Group, "acme", [reads, writes, cached, latency, duration]),
    ExpectedLines = [
        "# HELP acme_reads Total reads",
        "# TYPE acme_reads counter",
        "acme_reads{version=\"1.2.3\",component=\"thing1\"} 1",
        "acme_reads{component=\"thing2\",some_atom=\"atom_value\"} 3",
        "acme_reads{component=\"thing3\",some_binary=\"binary_value\"} 1234",
        "# HELP acme_writes Total writes",
        "# TYPE acme_writes counter",
        "acme_writes{version=\"1.2.3\",component=\"thing1\"} 2",
        "acme_writes{component=\"thing2\",some_atom=\"atom_value\"} 4",
        "acme_writes{component=\"thing3\",some_binary=\"binary_value\"} 4321",
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
    ExpectedResult = list_to_binary(string:join(ExpectedLines, "\n") ++ "\n"),

    ?assertEqual(ExpectedResult, Result),
    ok.

%% test helpers

set_value(Group, Id, Name, Value) ->
    [{Id, CRef, FieldSpec, _Labels}] = ets:lookup(seshat_counters_server:get_table(Group), Id),
    Fields = resolve_fieldspec(FieldSpec),
    {Name, Index, _Type, _Help} = lists:keyfind(Name, 1, Fields),
    counters:put(CRef, Index, Value).

resolve_fieldspec(Fields = FieldSpec) when is_list(FieldSpec) ->
    Fields;
resolve_fieldspec({persistent_term, PTerm}) ->
    persistent_term:get(PTerm).
