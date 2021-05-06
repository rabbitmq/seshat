%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(seshat_counters_server_SUITE).

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
    [
     get_table,
     get_tables,
     delete_table
    ].

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
get_table(_Config) ->
    Ref0 = seshat_counters_server:create_table("burrows"),
    Ref1 = seshat_counters_server:get_table("burrows"),
    ?assertEqual(Ref0, Ref1),
    ?assert(ets:info(Ref0) =/= undefined).

get_tables(_Config) ->
    seshat_counters_server:create_table("burrows"),
    seshat_counters_server:create_table("nests"),
    seshat_counters_server:create_table("roosts"),
    Tables = lists:sort(maps:keys(seshat_counters_server:get_tables())),
    ?assertMatch(Tables, lists:sort(["burrows", "nests", "roosts"])).

delete_table(_Config) ->
    seshat_counters_server:create_table("burrows"),
    seshat_counters_server:create_table("nests"),
    seshat_counters_server:create_table("roosts"),
    Tables0 = lists:sort(maps:keys(seshat_counters_server:get_tables())),
    ?assertMatch(Tables0, lists:sort(["burrows", "nests", "roosts"])),
    seshat_counters_server:delete_table("nests"),
    Tables = lists:sort(maps:keys(seshat_counters_server:get_tables())),
    ?assertMatch(Tables, lists:sort(["burrows", "roosts"])).
