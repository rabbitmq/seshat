%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(seshat_counters_test).
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
       fun prometheus_format_multiple_names/0 ]}.

overview() ->
    Group = "pets",
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
    seshat_counters:new_group(Group),
    seshat_counters:new(Group, "rabbit", Counters),
    Ref = seshat_counters:fetch(Group, "rabbit"),
    counters:add(Ref, 1, 3),
    counters:add(Ref, 2, 1),
    Overview = seshat_counters:overview(Group),
    ?assertEqual(
       #{"rabbit" => #{carrots_eaten_total => 3,
                       holes_dug_total => 1}},
       Overview).

prometheus_format_multiple_names() ->
    Group = people,
    Counters = [{ foo, 1, counter, "Total foos given" }],
    seshat_counters:new_group(Group),
    seshat_counters:new(Group, {name, you}, Counters),
    seshat_counters:new(Group, {name, me}, Counters),
    PrometheusFormat = seshat_counters:prometheus_format(Group),
    ExpectedPrometheusFormat = #{ foo => #{ type => counter,
                                            help => "Total foos given",
                                            values => #{{name, me} => 0,
                                                        {name, you} => 0} }},
    ?assertEqual(ExpectedPrometheusFormat, PrometheusFormat).
