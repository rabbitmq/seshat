%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2026 Broadcom. All Rights Reserved. The term Broadcom refers to Broadcom Inc. and/or its subsidiaries.
%%

%% A histogram metric type for seshat, backed by Erlang counters.
%%
%% A histogram is stored as a single counters array: one slot per bucket
%% plus a trailing slot holding the sum of every observed value. Histograms
%% are registered in the same group table as regular counters and are
%% exported through seshat:prom_format/2,3 alongside them.
%%
%% The write path mirrors the main seshat API: new/4,5 returns an opaque
%% reference which the caller is expected to hold on to (in process state
%% or in a persistent_term of its own choosing) and pass to observe/2.
%% fetch/2 recovers that reference from the group if it is lost.
-module(seshat_histogram).

-include("src/seshat.hrl").

-export([new/4,
         new/5,
         observe/2,
         fetch/2,
         buckets/2,
         overview/2,
         fold/3,
         delete/2,
         format/1,
         prom_format/2,
         prom_format/3]).

-record(histogram_ref, {cref :: counters:counters_ref(),
                        bounds :: tuple(),
                        sum_pos :: pos_integer()}).

-opaque histogram_ref() :: #histogram_ref{}.
-type bucket_counts() :: [{upper_bound(), non_neg_integer()}].
-type overview() :: #{buckets := bucket_counts(),
                      count := non_neg_integer(),
                      sum := non_neg_integer()}.
-type options() :: #{labels => labels_map(),
                     help => string()}.

%% One label set's worth of a metric family, as a Prometheus client
%% library expects it. Bucket counts are cumulative.
-type histogram_value() :: {[{label_name(), label_value()}],
                            bucket_counts(),
                            Count :: non_neg_integer(),
                            Sum :: non_neg_integer()}.
-type metric_families() :: #{atom() => #{type := histogram,
                                         help := string(),
                                         values := [histogram_value()]}}.

-export_type([histogram_ref/0, bucket_counts/0, overview/0, options/0,
              histogram_value/0, metric_families/0]).

%% @doc Create a new histogram in a group.
%%
%% @param Group the name of an existing group
%% @param Id the id this histogram is registered under within the group
%% @param Name the Prometheus metric family name
%% @param BucketBounds inclusive upper bounds
%% @returns an opaque reference for use with observe/2
-spec new(group(), id(), atom(), bucket_spec()) -> histogram_ref().
new(Group, Id, Name, BucketBounds) ->
    new(Group, Id, Name, BucketBounds, #{}).

%% @doc Create a new histogram in a group, with labels and help text.
%%
%% BucketBounds are inclusive upper bounds and need not be sorted;
%% `infinity' is appended if not already present. Duplicate bounds are
%% rejected, as they would produce two series with the same `le' label.
%%
%% Registering the same Id twice with identical arguments returns the
%% existing reference and leaves the accumulated observations intact, so
%% that initialisation is safe to repeat. Registering an Id that is already
%% in use with *different* arguments is an error: delete/2 first.
%%
%% @param Opts may carry `labels' and `help'
-spec new(group(), id(), atom(), bucket_spec(), options()) -> histogram_ref().
new(Group, Id, Name, BucketBounds, Opts)
  when is_atom(Name), is_map(Opts) ->
    Bounds = validate_bounds(BucketBounds),
    BoundsTuple = list_to_tuple(Bounds),
    SumPos = length(Bounds) + 1,
    Labels = maps:get(labels, Opts, #{}),
    Help = maps:get(help, Opts, ""),
    TRef = seshat_counters_server:get_table(Group),
    case ets:lookup(TRef, Id) of
        [#histogram_entry{bounds = BoundsTuple, name = Name,
                          help = Help, labels = Labels} = Existing] ->
            %% identical re-registration: hand back the existing counters
            %% rather than silently discarding what has been observed
            to_ref(Existing);
        [#histogram_entry{}] ->
            error({histogram_already_registered, Id});
        [#entry{}] ->
            error({id_already_registered, Id});
        [] ->
            CRef = counters:new(SumPos, [write_concurrency]),
            Entry = #histogram_entry{
                       id = Id,
                       cref = CRef,
                       bounds = BoundsTuple,
                       sum_pos = SumPos,
                       name = Name,
                       help = Help,
                       labels = Labels,
                       rendered_labels = seshat:labels_to_binary(Labels)},
            true = ets:insert(TRef, Entry),
            to_ref(Entry)
    end.

%% @doc Record an observation.
%%
%% Values must be non-negative integers. The backing storage is an Erlang
%% counters array, which cannot hold floats, so a quantity that is naturally
%% fractional must be observed in a smaller unit -- milliseconds or
%% microseconds rather than seconds. Bucket bounds carry the same
%% restriction, and the exported `_sum' is likewise an integer.
-spec observe(histogram_ref(), non_neg_integer()) -> ok.
observe(#histogram_ref{cref = CRef, bounds = Bounds, sum_pos = SumPos}, Value)
  when is_integer(Value), Value >= 0 ->
    Pos = find_bucket(Value, Bounds, 1),
    counters:add(CRef, Pos, 1),
    counters:add(CRef, SumPos, Value),
    ok.

%% @doc Return a reference to an existing histogram.
%%
%% fetch/2 is NOT meant to be called for every observation. Like
%% seshat:fetch/2, it exists so that a caller which has lost the reference
%% returned by new/4,5 can recover it without re-registering the histogram.
%%
%% @returns 'undefined' if Id is unknown or names a metric set rather than
%% a histogram
-spec fetch(group(), id()) -> undefined | histogram_ref().
fetch(Group, Id) ->
    TRef = seshat_counters_server:get_table(Group),
    try ets:lookup(TRef, Id) of
        [#histogram_entry{} = Entry] ->
            to_ref(Entry);
        _ ->
            undefined
    catch
        error:badarg ->
            undefined
    end.

%% @doc Return the non-cumulative bucket counts for a histogram.
%%
%% Note that Prometheus exposition uses *cumulative* bucket counts;
%% prom_format/2,3 accumulates these.
-spec buckets(group(), id()) -> bucket_counts() | undefined.
buckets(Group, Id) ->
    case ets:lookup(seshat_counters_server:get_table(Group), Id) of
        [#histogram_entry{cref = CRef, bounds = Bounds, sum_pos = SumPos}] ->
            read_buckets(CRef, Bounds, SumPos);
        _ ->
            undefined
    end.

%% @doc Return the buckets, observation count and sum for a histogram.
-spec overview(group(), id()) -> overview() | undefined.
overview(Group, Id) ->
    case ets:lookup(seshat_counters_server:get_table(Group), Id) of
        [#histogram_entry{} = Entry] ->
            entry_overview(Entry);
        _ ->
            undefined
    end.

%% @doc Fold over every histogram in a group.
%% Fun receives the id, its overview, and the accumulator.
-spec fold(Fun, Acc0 :: term(), group()) -> term() when
      Fun :: fun((id(), overview(), term()) -> term()).
fold(Fun, Acc0, Group) ->
    ets:foldl(fun (#histogram_entry{id = Id} = Entry, Acc) ->
                      Fun(Id, entry_overview(Entry), Acc);
                  (#entry{}, Acc) ->
                      Acc
              end, Acc0, seshat_counters_server:get_table(Group)).

%% @doc Delete a histogram from a group.
%%
%% Histograms share the group's id namespace with metric sets, so this is
%% seshat:delete/2 and either function will delete either kind of entry.
-spec delete(group(), id()) -> ok.
delete(Group, Id) ->
    seshat:delete(Group, Id).

%% @doc Emit Prometheus exposition format for every histogram in a group.
%%
%% Normally reached through seshat:prom_format/2,3, which renders a group's
%% metric sets and histograms together.
%%
%% Unlike the metric-set path, histograms without labels are emitted rather
%% than skipped.
-spec prom_format(group(), string()) -> iodata().
prom_format(Group, Prefix) ->
    prom_format(Group, Prefix, all).

-spec prom_format(group(), string(), all | [atom()]) -> iodata().
prom_format(Group, Prefix, Metrics) ->
    PrefixBin = case unicode:characters_to_binary(Prefix ++ "_") of
                    P when is_tuple(P) ->
                        %% characters_to_binary errors are tuples
                        <<>>;
                    P -> P
                end,
    maps:fold(fun (Name, Entries, Acc) ->
                      [Acc, format_family(PrefixBin, Name, Entries)]
              end, [], families(Group, Metrics)).

%% @doc Return every histogram in a group as a map of metric families.
%%
%% This is the histogram counterpart to seshat:format/2: it hands back the
%% data a Prometheus client library needs to build a metric family itself,
%% where prom_format/2,3 renders the exposition text directly.
%%
%% Bucket counts are cumulative, as Prometheus requires and unlike the
%% non-cumulative counts buckets/2 returns. Labels are a proplist rather
%% than a map, which is the shape client libraries take.
-spec format(group()) -> metric_families().
format(Group) ->
    maps:map(fun (_Name, [#histogram_entry{help = Help} | _] = Entries) ->
                     #{type => histogram,
                       help => Help,
                       values => [entry_values(E) || E <- Entries]}
             end, families(Group, all)).

%%%===================================================================
%%% Internal
%%%===================================================================

%% Several histograms may share a metric family name, differing only in
%% their labels, so entries are grouped by name: an exposition that
%% declares a family more than once is rejected, and a client library
%% likewise wants one family carrying many label sets.
families(Group, Metrics) ->
    ets:foldl(
      fun (#histogram_entry{name = Name} = Entry, Acc) ->
              case is_selected(Name, Metrics) of
                  true ->
                      maps:update_with(
                        Name, fun (Es) -> [Entry | Es] end, [Entry], Acc);
                  false ->
                      Acc
              end;
          (#entry{}, Acc) ->
              Acc
      end, #{}, seshat_counters_server:get_table(Group)).

entry_values(#histogram_entry{cref = CRef, bounds = Bounds, sum_pos = SumPos,
                              labels = Labels}) ->
    {Buckets, Count} = cumulative(read_buckets(CRef, Bounds, SumPos)),
    {maps:to_list(Labels), Buckets, Count, counters:get(CRef, SumPos)}.

%% Prometheus buckets are cumulative: each carries the count of every
%% observation less than or equal to its upper bound, so the final
%% 'infinity' bucket holds the total. Returns that total alongside.
cumulative(Buckets) ->
    lists:mapfoldl(fun ({UpperBound, N}, Acc0) ->
                           Acc = Acc0 + N,
                           {{UpperBound, Acc}, Acc}
                   end, 0, Buckets).

to_ref(#histogram_entry{cref = CRef, bounds = Bounds, sum_pos = SumPos}) ->
    #histogram_ref{cref = CRef, bounds = Bounds, sum_pos = SumPos}.

entry_overview(#histogram_entry{cref = CRef, bounds = Bounds,
                                sum_pos = SumPos}) ->
    Buckets = read_buckets(CRef, Bounds, SumPos),
    #{buckets => Buckets,
      count => bucket_total(Buckets),
      sum => counters:get(CRef, SumPos)}.

bucket_total(Buckets) ->
    lists:foldl(fun ({_, N}, Acc) -> Acc + N end, 0, Buckets).

validate_bounds(Bounds) when is_list(Bounds) ->
    case lists:all(fun (infinity) -> true;
                       (B) -> is_integer(B) andalso B >= 0
                   end, Bounds) of
        true ->
            %% atoms sort after all integers in Erlang term order, so
            %% 'infinity' lands last
            Sorted = lists:sort(Bounds),
            case lists:usort(Bounds) of
                Sorted ->
                    ensure_infinity(Sorted);
                _ ->
                    error({duplicate_bucket_bounds, Bounds})
            end;
        false ->
            error({invalid_bucket_bounds, Bounds})
    end;
validate_bounds(Bounds) ->
    error({invalid_bucket_bounds, Bounds}).

ensure_infinity([]) ->
    [infinity];
ensure_infinity(Bounds) ->
    case lists:last(Bounds) of
        infinity -> Bounds;
        _ -> Bounds ++ [infinity]
    end.

find_bucket(_, Bounds, Pos) when Pos > tuple_size(Bounds) ->
    tuple_size(Bounds);
find_bucket(Value, Bounds, Pos) ->
    case element(Pos, Bounds) of
        infinity -> Pos;
        Bound when Value =< Bound -> Pos;
        _ -> find_bucket(Value, Bounds, Pos + 1)
    end.

read_buckets(CRef, Bounds, SumPos) ->
    NumBuckets = SumPos - 1,
    [{element(I, Bounds), counters:get(CRef, I)}
     || I <- lists:seq(1, NumBuckets)].

is_selected(_Name, all) ->
    true;
is_selected(Name, Names) when is_list(Names) ->
    lists:member(Name, Names).

%% HELP and TYPE are declared on the family's base name; the _bucket, _sum
%% and _count series hang off it.
format_family(PrefixBin, Name, [#histogram_entry{help = Help} | _] = Entries) ->
    NameBin = <<PrefixBin/binary, (atom_to_binary(Name, utf8))/binary>>,
    [["# HELP ", NameBin, " ", Help, "\n"],
     ["# TYPE ", NameBin, " histogram\n"],
     [format_series(NameBin, Entry) || Entry <- Entries]].

format_series(NameBin, #histogram_entry{cref = CRef, bounds = Bounds,
                                        sum_pos = SumPos,
                                        rendered_labels = Labels}) ->
    BucketName = <<NameBin/binary, "_bucket">>,
    {Buckets, Count} = cumulative(read_buckets(CRef, Bounds, SumPos)),
    BucketLines =
        [begin
             Le = upper_bound_to_binary(UpperBound),
             LeLabel = case Labels of
                           <<>> ->
                               <<"le=\"", Le/binary, "\"">>;
                           _ ->
                               <<Labels/binary, ",le=\"", Le/binary, "\"">>
                       end,
             [BucketName, "{", LeLabel, "} ", integer_to_binary(Cum), "\n"]
         end || {UpperBound, Cum} <- Buckets],
    Sum = counters:get(CRef, SumPos),
    [BucketLines,
     format_line(NameBin, <<"_sum">>, Labels, Sum),
     format_line(NameBin, <<"_count">>, Labels, Count)].

upper_bound_to_binary(infinity) -> <<"+Inf">>;
upper_bound_to_binary(N) -> integer_to_binary(N).

format_line(Name, Suffix, <<>>, Value) ->
    [Name, Suffix, " ", integer_to_binary(Value), "\n"];
format_line(Name, Suffix, Labels, Value) ->
    [Name, Suffix, "{", Labels, "} ", integer_to_binary(Value), "\n"].
