%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2025 Broadcom. All Rights Reserved. The term Broadcom refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(seshat_counters_server_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [get_table, get_tables, delete_table].

init_per_suite(Config) ->
    logger:remove_handler(default),
    {ok, _} = application:ensure_all_started(seshat),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(seshat).

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test cases

get_table(_Config) ->
    CreatedTable = seshat_counters_server:create_table("burrows"),
    QueriedTable = seshat_counters_server:get_table("burrows"),
    CreatedTable = QueriedTable,
    ?assertNotEqual(undefined, ets:info(CreatedTable)).

get_tables(_Config) ->
    seshat_counters_server:create_table("burrows"),
    seshat_counters_server:create_table("nests"),
    Tables = lists:sort(maps:keys(seshat_counters_server:get_tables())),
    ?assertEqual(["burrows", "nests"], Tables).

delete_table(_Config) ->
    seshat_counters_server:create_table("burrows"),
    seshat_counters_server:create_table("nests"),
    Tables = lists:sort(maps:keys(seshat_counters_server:get_tables())),
    ?assertEqual(["burrows", "nests"], Tables),
    seshat_counters_server:delete_table("nests"),
    seshat_counters_server:delete_table("nests"),
    Tables1 = lists:sort(maps:keys(seshat_counters_server:get_tables())),
    ?assertEqual(["burrows"], Tables1).
