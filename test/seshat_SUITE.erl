%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(seshat_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-export([]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [{group, tests}].

all_tests() ->
    [counters].

groups() ->
    [{tests, [], all_tests()}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, Apps} = application:ensure_all_started(seshat),
    [{started_apps, Apps} | Config].

end_per_testcase(_TestCase, Config) ->
    [application:stop(App)
     || App <- lists:reverse(?config(started_apps, Config))],
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================
counters(_Config) ->
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
    ?assertMatch(#{"rabbit" := #{carrots_eaten_total := 3,holes_dug_total := 1}},
                 Overview).
