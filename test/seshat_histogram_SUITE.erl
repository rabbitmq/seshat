%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2026 Broadcom. All Rights Reserved. The term Broadcom refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(seshat_histogram_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").

-define(GROUP, test_histograms).

all() ->
    [new_creates_histogram,
     observe_increments_correct_bucket,
     observe_tracks_sum,
     overview_returns_all_fields,
     overview_of_unknown_id_is_undefined,
     infinity_appended_if_missing,
     bounds_are_sorted,
     empty_bounds_gives_catch_all_bucket,
     duplicate_bounds_rejected,
     invalid_bounds_rejected,
     identical_registration_is_idempotent,
     conflicting_registration_errors,
     registration_over_a_counter_errors,
     fetch_returns_working_ref,
     fetch_unknown_id_is_undefined,
     fetch_does_not_confuse_counters_and_histograms,
     delete_removes_histogram,
     fold_over_histograms,
     counters_api_ignores_histograms,
     prom_format_basic,
     prom_format_with_labels,
     prom_format_help_and_type_once_per_family,
     prom_format_names_filter,
     prom_format_includes_histograms,
     format_returns_metric_families,
     format_buckets_are_cumulative,
     format_groups_label_sets_into_one_family,
     format_of_group_without_histograms_is_empty].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(seshat),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(seshat),
    ok.

init_per_testcase(_TC, Config) ->
    seshat:new_group(?GROUP),
    Config.

end_per_testcase(_TC, _Config) ->
    seshat:delete_group(?GROUP),
    ok.

%% Returns the reason of the error Fun raises, so that it can be matched
%% against a pattern.
error_reason(Fun) ->
    try Fun() of
        %% raised from the 'of' body, so not caught below
        Value -> error({expected_error, {returned, Value}})
    catch
        error:Reason -> Reason
    end.

new_creates_histogram(_Config) ->
    _Ref = seshat_histogram:new(?GROUP, hist1, entry_bytes, [100, 1000, 10000]),
    [{100, 0}, {1000, 0}, {10000, 0}, {infinity, 0}] =
        seshat_histogram:buckets(?GROUP, hist1).

observe_increments_correct_bucket(_Config) ->
    Ref = seshat_histogram:new(?GROUP, hist1, entry_bytes, [100, 1000, infinity]),
    ok = seshat_histogram:observe(Ref, 50),
    ok = seshat_histogram:observe(Ref, 100),
    ok = seshat_histogram:observe(Ref, 500),
    ok = seshat_histogram:observe(Ref, 5000),
    [{100, 2}, {1000, 1}, {infinity, 1}] =
        seshat_histogram:buckets(?GROUP, hist1).

observe_tracks_sum(_Config) ->
    Ref = seshat_histogram:new(?GROUP, hist1, entry_bytes, [100, 1000, infinity]),
    ok = seshat_histogram:observe(Ref, 50),
    ok = seshat_histogram:observe(Ref, 200),
    #{sum := 250} = seshat_histogram:overview(?GROUP, hist1).

overview_returns_all_fields(_Config) ->
    Ref = seshat_histogram:new(?GROUP, hist1, entry_bytes, [100, 1000, infinity]),
    ok = seshat_histogram:observe(Ref, 50),
    ok = seshat_histogram:observe(Ref, 200),
    #{buckets := [{100, 1}, {1000, 1}, {infinity, 0}],
      count := 2,
      sum := 250} = seshat_histogram:overview(?GROUP, hist1).

overview_of_unknown_id_is_undefined(_Config) ->
    undefined = seshat_histogram:overview(?GROUP, no_such_hist),
    undefined = seshat_histogram:buckets(?GROUP, no_such_hist).

infinity_appended_if_missing(_Config) ->
    _Ref = seshat_histogram:new(?GROUP, hist1, entry_bytes, [100, 1000]),
    Buckets = seshat_histogram:buckets(?GROUP, hist1),
    {infinity, 0} = lists:last(Buckets).

bounds_are_sorted(_Config) ->
    Ref = seshat_histogram:new(?GROUP, hist1, entry_bytes, [1000, 10, 100]),
    ok = seshat_histogram:observe(Ref, 50),
    [{10, 0}, {100, 1}, {1000, 0}, {infinity, 0}] =
        seshat_histogram:buckets(?GROUP, hist1).

empty_bounds_gives_catch_all_bucket(_Config) ->
    Ref = seshat_histogram:new(?GROUP, hist1, entry_bytes, []),
    ok = seshat_histogram:observe(Ref, 12345),
    #{buckets := [{infinity, 1}], count := 1, sum := 12345} =
        seshat_histogram:overview(?GROUP, hist1).

duplicate_bounds_rejected(_Config) ->
    {duplicate_bucket_bounds, _} =
        error_reason(fun () ->
                             seshat_histogram:new(?GROUP, hist1, entry_bytes,
                                                  [10, 100, 10])
                     end).

invalid_bounds_rejected(_Config) ->
    Reject = fun (Bounds) ->
                     error_reason(fun () ->
                                          seshat_histogram:new(?GROUP, hist1,
                                                               entry_bytes,
                                                               Bounds)
                                  end)
             end,
    {invalid_bucket_bounds, _} = Reject([10, -1]),
    {invalid_bucket_bounds, _} = Reject([10, nope]),
    {invalid_bucket_bounds, _} = Reject(not_a_list).

%% Re-running an application's metric initialisation must not silently
%% discard everything observed so far.
identical_registration_is_idempotent(_Config) ->
    Opts = #{labels => #{protocol => amqp091}, help => "Entry size"},
    Ref = seshat_histogram:new(?GROUP, hist1, entry_bytes, [100, 1000], Opts),
    ok = seshat_histogram:observe(Ref, 50),
    Ref2 = seshat_histogram:new(?GROUP, hist1, entry_bytes, [100, 1000], Opts),
    ok = seshat_histogram:observe(Ref2, 50),
    #{count := 2, sum := 100} = seshat_histogram:overview(?GROUP, hist1).

conflicting_registration_errors(_Config) ->
    _ = seshat_histogram:new(?GROUP, hist1, entry_bytes, [100, 1000]),
    {histogram_already_registered, hist1} =
        error_reason(fun () ->
                             seshat_histogram:new(?GROUP, hist1, entry_bytes,
                                                  [1, 2, 3])
                     end),
    {histogram_already_registered, hist1} =
        error_reason(fun () ->
                             seshat_histogram:new(?GROUP, hist1, other_name,
                                                  [100, 1000])
                     end).

registration_over_a_counter_errors(_Config) ->
    Fields = [{requests_total, 1, counter, "Total requests"}],
    _ = seshat:new(?GROUP, shared_id, Fields, #{service => web}),
    {id_already_registered, shared_id} =
        error_reason(fun () ->
                             seshat_histogram:new(?GROUP, shared_id,
                                                  entry_bytes, [100])
                     end).

fetch_returns_working_ref(_Config) ->
    Ref = seshat_histogram:new(?GROUP, hist1, entry_bytes, [100, infinity]),
    ok = seshat_histogram:observe(Ref, 50),
    %% recover the ref as a restarting caller would
    Fetched = seshat_histogram:fetch(?GROUP, hist1),
    ok = seshat_histogram:observe(Fetched, 50),
    #{count := 2, sum := 100} = seshat_histogram:overview(?GROUP, hist1).

fetch_unknown_id_is_undefined(_Config) ->
    undefined = seshat_histogram:fetch(?GROUP, no_such_hist).

fetch_does_not_confuse_counters_and_histograms(_Config) ->
    Fields = [{requests_total, 1, counter, "Total requests"}],
    CRef = seshat:new(?GROUP, a_counter, Fields, #{service => web}),
    _ = seshat_histogram:new(?GROUP, a_histogram, entry_bytes, [100]),
    counters:add(CRef, 1, 7),
    %% fetch/2 finds the same counters array. Checked by reading through
    %% what it returns rather than by comparing the two references: a
    %% counters_ref() is opaque, and dialyzer rejects comparing one from
    %% OTP 28 on.
    case seshat:fetch(?GROUP, a_counter) of
        undefined ->
            error(counter_not_found);
        Fetched ->
            7 = counters:get(Fetched, 1)
    end,
    %% each fetch only sees its own kind of entry
    undefined = seshat:fetch(?GROUP, a_histogram),
    undefined = seshat_histogram:fetch(?GROUP, a_counter),
    true = undefined =/= seshat_histogram:fetch(?GROUP, a_histogram).

delete_removes_histogram(_Config) ->
    _ = seshat_histogram:new(?GROUP, hist1, entry_bytes, [100]),
    ok = seshat_histogram:delete(?GROUP, hist1),
    undefined = seshat_histogram:fetch(?GROUP, hist1),
    %% the id is free again
    _ = seshat_histogram:new(?GROUP, hist1, entry_bytes, [1, 2, 3]),
    ok.

fold_over_histograms(_Config) ->
    RefA = seshat_histogram:new(?GROUP, hist_a, entry_bytes, [100, infinity]),
    RefB = seshat_histogram:new(?GROUP, hist_b, chunk_bytes, [100, infinity]),
    %% a counter in the same group must not be folded over
    Fields = [{requests_total, 1, counter, "Total requests"}],
    _ = seshat:new(?GROUP, a_counter, Fields, #{service => web}),
    ok = seshat_histogram:observe(RefA, 50),
    ok = seshat_histogram:observe(RefB, 200),
    Folded = seshat_histogram:fold(fun (Id, Overview, Acc) ->
                                           Acc#{Id => Overview}
                                   end, #{}, ?GROUP),
    2 = map_size(Folded),
    #{hist_a := #{count := 1, sum := 50},
      hist_b := #{count := 1, sum := 200}} = Folded.

counters_api_ignores_histograms(_Config) ->
    Fields = [{requests_total, 1, counter, "Total requests"}],
    CRef = seshat:new(?GROUP, a_counter, Fields, #{service => web}),
    counters:add(CRef, 1, 42),
    _ = seshat_histogram:new(?GROUP, a_histogram, entry_bytes, [100]),
    #{a_counter := #{requests_total := 42}} = All = seshat:counters(?GROUP),
    1 = map_size(All),
    undefined = seshat:counters(?GROUP, a_histogram),
    %% format/2 covers metric sets only
    Formatted = seshat:format(?GROUP),
    [<<"requests_total">>] = lists:sort(maps:keys(Formatted)).

prom_format_basic(_Config) ->
    Ref = seshat_histogram:new(?GROUP, hist1, entry_bytes,
                               [100, 1000, infinity],
                               #{help => "Entry size in bytes"}),
    ok = seshat_histogram:observe(Ref, 50),
    ok = seshat_histogram:observe(Ref, 500),
    Output = iolist_to_binary(seshat:prom_format(?GROUP, "osiris")),
    %% HELP and TYPE hang off the family's base name, not off _bucket
    {match, _} = re:run(Output, <<"# TYPE osiris_entry_bytes histogram">>),
    {match, _} = re:run(Output, <<"# HELP osiris_entry_bytes Entry size in bytes">>),
    nomatch = re:run(Output, <<"# TYPE osiris_entry_bytes_bucket">>),
    {match, _} = re:run(Output, <<"osiris_entry_bytes_bucket\\{le=\"100\"\\} 1">>),
    {match, _} = re:run(Output, <<"osiris_entry_bytes_bucket\\{le=\"1000\"\\} 2">>),
    {match, _} = re:run(Output, <<"osiris_entry_bytes_bucket\\{le=\"\\+Inf\"\\} 2">>),
    {match, _} = re:run(Output, <<"osiris_entry_bytes_sum 550">>),
    {match, _} = re:run(Output, <<"osiris_entry_bytes_count 2">>).

prom_format_with_labels(_Config) ->
    Ref = seshat_histogram:new(?GROUP, hist1, msg_size_bytes,
                               [100, infinity],
                               #{labels => #{protocol => amqp091},
                                 help => "Message size"}),
    ok = seshat_histogram:observe(Ref, 50),
    Output = iolist_to_binary(seshat:prom_format(?GROUP, "rabbitmq")),
    {match, _} = re:run(Output,
                        <<"rabbitmq_msg_size_bytes_bucket\\{protocol=\"amqp091\",le=\"100\"\\} 1">>),
    {match, _} = re:run(Output,
                        <<"rabbitmq_msg_size_bytes_sum\\{protocol=\"amqp091\"\\} 50">>),
    {match, _} = re:run(Output,
                        <<"rabbitmq_msg_size_bytes_count\\{protocol=\"amqp091\"\\} 1">>).

%% Two histograms sharing a metric family name, differing only by label:
%% Prometheus rejects a scrape that declares HELP or TYPE twice for a family.
prom_format_help_and_type_once_per_family(_Config) ->
    RefA = seshat_histogram:new(?GROUP, hist_a, msg_size_bytes, [100, infinity],
                                #{labels => #{protocol => amqp091},
                                  help => "Message size"}),
    RefB = seshat_histogram:new(?GROUP, hist_b, msg_size_bytes, [100, infinity],
                                #{labels => #{protocol => amqp10},
                                  help => "Message size"}),
    ok = seshat_histogram:observe(RefA, 50),
    ok = seshat_histogram:observe(RefB, 500),
    Output = iolist_to_binary(seshat:prom_format(?GROUP, "rabbitmq")),
    {match, Helps} = re:run(Output, <<"# HELP rabbitmq_msg_size_bytes ">>,
                            [global]),
    1 = length(Helps),
    {match, Types} = re:run(Output, <<"# TYPE rabbitmq_msg_size_bytes ">>,
                            [global]),
    1 = length(Types),
    %% but both label sets are present
    {match, _} = re:run(Output,
                        <<"rabbitmq_msg_size_bytes_count\\{protocol=\"amqp091\"\\} 1">>),
    {match, _} = re:run(Output,
                        <<"rabbitmq_msg_size_bytes_count\\{protocol=\"amqp10\"\\} 1">>).

prom_format_names_filter(_Config) ->
    RefA = seshat_histogram:new(?GROUP, hist_a, entry_bytes, [100, infinity]),
    RefB = seshat_histogram:new(?GROUP, hist_b, chunk_bytes, [100, infinity]),
    ok = seshat_histogram:observe(RefA, 50),
    ok = seshat_histogram:observe(RefB, 50),
    Output = iolist_to_binary(seshat:prom_format(?GROUP, "osiris", [entry_bytes])),
    {match, _} = re:run(Output, <<"osiris_entry_bytes_bucket">>),
    nomatch = re:run(Output, <<"osiris_chunk_bytes">>).

%% Verify that seshat:prom_format includes both counters and histograms
prom_format_includes_histograms(_Config) ->
    %% Register a regular counter
    Fields = [{requests_total, 1, counter, "Total requests"}],
    CRef = seshat:new(?GROUP, my_service, Fields, #{service => web}),
    counters:add(CRef, 1, 42),
    %% Register a histogram
    Ref = seshat_histogram:new(?GROUP, hist1, latency_ms,
                               [10, 100, infinity],
                               #{help => "Request latency"}),
    ok = seshat_histogram:observe(Ref, 55),
    %% Single prom_format call gets both
    Output = iolist_to_binary(seshat:prom_format(?GROUP, "app")),
    {match, _} = re:run(Output, <<"app_requests_total">>),
    {match, _} = re:run(Output, <<"app_latency_ms_bucket">>).

format_returns_metric_families(_Config) ->
    Ref = seshat_histogram:new(?GROUP, hist1, msg_size_bytes,
                               [100, 1000, infinity],
                               #{labels => #{protocol => amqp091},
                                 help => "Message size"}),
    ok = seshat_histogram:observe(Ref, 50),
    ok = seshat_histogram:observe(Ref, 500),
    #{msg_size_bytes := #{type := histogram,
                          help := "Message size",
                          values := [Value]}} = seshat_histogram:format(?GROUP),
    {Labels, Buckets, Count, Sum} = Value,
    %% labels are a proplist, which is the shape client libraries take
    [{protocol, amqp091}] = Labels,
    [{100, 1}, {1000, 2}, {infinity, 2}] = Buckets,
    2 = Count,
    550 = Sum,
    ok.

%% buckets/2 reports non-cumulative counts; format/1 must not.
format_buckets_are_cumulative(_Config) ->
    Ref = seshat_histogram:new(?GROUP, hist1, entry_bytes, [10, 100, 1000]),
    ok = seshat_histogram:observe(Ref, 5),
    ok = seshat_histogram:observe(Ref, 50),
    ok = seshat_histogram:observe(Ref, 5000),
    [{10, 1}, {100, 1}, {1000, 0}, {infinity, 1}] =
        seshat_histogram:buckets(?GROUP, hist1),
    #{entry_bytes := #{values := [{[], Buckets, 3, 5055}]}} =
        seshat_histogram:format(?GROUP),
    [{10, 1}, {100, 2}, {1000, 2}, {infinity, 3}] = Buckets,
    ok.

%% Two histograms sharing a name are one family carrying two label sets,
%% not two families.
format_groups_label_sets_into_one_family(_Config) ->
    RefA = seshat_histogram:new(?GROUP, hist_a, msg_size_bytes, [100, infinity],
                                #{labels => #{protocol => amqp091},
                                  help => "Message size"}),
    RefB = seshat_histogram:new(?GROUP, hist_b, msg_size_bytes, [100, infinity],
                                #{labels => #{protocol => amqp10},
                                  help => "Message size"}),
    ok = seshat_histogram:observe(RefA, 50),
    ok = seshat_histogram:observe(RefB, 500),
    Formatted = seshat_histogram:format(?GROUP),
    1 = map_size(Formatted),
    #{msg_size_bytes := #{values := Values}} = Formatted,
    2 = length(Values),
    [{[{protocol, amqp091}], _, 1, 50},
     {[{protocol, amqp10}], _, 1, 500}] = lists:sort(Values),
    ok.

format_of_group_without_histograms_is_empty(_Config) ->
    Fields = [{requests_total, 1, counter, "Total requests"}],
    _ = seshat:new(?GROUP, a_counter, Fields, #{service => web}),
    Formatted = seshat_histogram:format(?GROUP),
    0 = map_size(Formatted),
    ok.
