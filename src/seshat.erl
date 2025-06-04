%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2025 Broadcom. All Rights Reserved. The term Broadcom refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(seshat).

-include("src/seshat.hrl").

-export([new_group/1,
         delete_group/1,
         new/3,
         new/4,
         fetch/2,
         overview/1,
         overview/2,
         counters/1,
         counters/2,
         counters/3,
         delete/2,
         format/1,
         format/2,
         prom_format/2,
         prom_format/3,
         resolve_fields_spec/1
        ]).

-deprecated({overview, 1}).
-deprecated({overview, 2}).

-define(DEFAULT_FORMAT_OPTIONS, #{metrics => all,
                                  labels => as_map,
                                  filter_fun => fun(_) -> true end}).

%% @doc Create a new empty group of metrics.
%% Each group is completely isolated.
%%
%% @param Group the name of the group
%% @returns a reference to the new group
-spec new_group(group()) -> group_ref().
new_group(Group) ->
    seshat_counters_server:create_table(Group).

%% @doc Delete an existing group.
%%
%% @param Group the name of the group
%% @returns 'ok'
-spec delete_group(group()) -> ok.
delete_group(Group) ->
    seshat_counters_server:delete_table(Group).

%% @doc Create a new set of metrics.
%% A set of metrics is stored in a counter (@see counters)
%%
%% @param Group the name of an existing group
%% @param Id the id of an object these metrics are assosiated with
%% @param FieldsSpec metadata for the values stored in the counter
%% @returns a reference to the counter
-spec new(group(), id(), fields_spec()) ->
    counters:counters_ref().
new(Group, Id, FieldsSpec)  ->
    new(Group, Id, FieldsSpec, #{}).

%% @doc Create a new set of metrics.
%% A set of metrics is stored in a counter (@see counters)
%%
%% @param Group the name of an existing group
%% @param Id the id of an object these metrics are assosiated with
%% @param FieldsSpec metadata for the values stored in the counter
%% @param Labels key-value pairs describing the object
%% @returns a reference to the counter
-spec new(group(), id(), fields_spec(), labels()) ->
    counters:counters_ref().
new(Group, Id, Fields = FieldsSpec, Labels) when is_list(Fields) ->
    new_counter(Group, Id, Fields, FieldsSpec, Labels);
new(Group, Id, {persistent_term, PTerm} = FieldsSpec, Labels) ->
    case persistent_term:get(PTerm, undefined) of
        undefined ->
            error({non_existent_fields_spec, FieldsSpec});
        Fields ->
            new_counter(Group, Id, Fields, FieldsSpec, Labels)
    end.

%% @doc Return a reference to an existing set of metrics.
%% fetch/2 is NOT meant to be called for every counter update.
%% Instead, for higher performance, the consuming application should
%% store the returned counters_ref in a stateful Erlang module or in
%% persistent_term (@see persistent:term_put/2).
%%
%% @param Group the name of an existing group
%% @param Id the id of an existing object
%% @returns a reference to the counter
-spec fetch(group(), id()) -> undefined | counters:counters_ref().
fetch(Group, Id) ->
    TRef = seshat_counters_server:get_table(Group),
    try
        ets:lookup_element(TRef, Id, #entry.cref)
    catch
        error:badarg ->
            undefined
    end.

%% @doc Delete a metric set
%%
%% @param Group the name of an existing group
%% @param Id the id of an existing object
%% @returns 'ok'
-spec delete(group(), id()) -> ok.
delete(Group, Id) ->
    TRef = seshat_counters_server:get_table(Group),
    true = ets:delete(TRef, Id),
    ok.

%% Use counters/1 instead
-spec overview(group()) ->
    #{id() => #{atom() => integer()}}.
overview(Group) ->
    counters(Group).

%% Use counters/2 instead
-spec overview(group(), id()) ->
    #{atom() => integer()} | undefined.
overview(Group, Id) ->
    counters(Group, Id).

%% Helper function to build the map of counters for a given CRef and FieldSpec
-spec build_counters_map(counters:counters_ref(), fields_spec()) ->
    #{atom() => integer()}.
build_counters_map(CRef, FieldsSpec) ->
    Fields = resolve_fields_spec(FieldsSpec),
    lists:foldl(fun ({Name, Index, _Type, _Help}, Acc0) ->
                    Acc0#{Name => counters:get(CRef, Index)}
                end, #{}, Fields).

%% @doc Return a map with all metrics of all objects in the group
%% The returned map has the following structure:
%% #{Id => #{Name => Value}}
%%
%% @param Group the name of an existing group
-spec counters(group()) ->
    #{id() => #{atom() => integer()}}.
counters(Group) ->
    ets:foldl(
      fun(#entry{id = Id, cref = CRef, field_spec = FieldsSpec}, Acc) ->
              CountersMap = build_counters_map(CRef, FieldsSpec),
              Acc#{Id => CountersMap}
      end, #{}, seshat_counters_server:get_table(Group)).

%% @doc Return a map with all metrics for the object
%% The returned map has the following structure:
%% #{Name => Value}
%%
%% @param Group the name of an existing group
%% @param Id the name of an existing object
-spec counters(group(), id()) ->
    #{atom() => integer()} | undefined.
counters(Group, Id) ->
    case ets:lookup(seshat_counters_server:get_table(Group), Id) of
        [#entry{cref = CRef, field_spec = FieldsSpec}] ->
            build_counters_map(CRef, FieldsSpec);
        _ ->
            undefined
    end.

%% @doc Return a map with selected metrics for the object
%% The returned map has the following structure:
%% #{Name => Value}
%%
%% @param Group the name of an existing group
%% @param Id the name of an existing object
%% @param Names the list of metrics to return
-spec counters(group(), id(), [atom()]) ->
    #{atom() => integer()} | undefined.
counters(Group, Id, Names) ->
    case ets:lookup(seshat_counters_server:get_table(Group), Id) of
        [#entry{cref = CRef, field_spec = FieldsSpec}] ->
            AllCountersMap = build_counters_map(CRef, FieldsSpec),
            maps:with(Names, AllCountersMap);
        _ ->
            undefined
    end.

%% @doc Return a map with all metrics for all objects in the group
%% The returned map has the following structure:
%% #{Name => #{LabelMap => Value}}
%% This structure is similar to what Prometheus expects,
%% with label sets associated with metric values.
%%
%% @param Group the name of an existing group
-spec format(group()) -> format_result().
format(Group) ->
    format(Group, ?DEFAULT_FORMAT_OPTIONS).

%% @doc Return a map with all or selected metrics for all objects in the group
%% The returned map has the following structure:
%% #{Name => #{Labels => Value}}
%% Labels can be a map or as a binary (optimization).
%%
%% @param Group the name of an existing group
%% @param Options formatting options
-spec format(group(), format_options()) -> format_result().
format(Group, Options) ->
    #{metrics := Metrics,
      labels := LabelFormat,
      filter_fun := FilterFun} = maps:merge(?DEFAULT_FORMAT_OPTIONS, Options),
    ets:foldl(fun
                  (#entry{cref = CRef,
                          field_spec = FieldsSpec,
                          labels = LabelsAsMap,
                          rendered_labels = LabelsAsBinary}, Acc) ->
                      case map_size(LabelsAsMap) > 0 andalso FilterFun(LabelsAsMap) of
                          true ->
                              Fields0 = resolve_fields_spec(FieldsSpec),
                              Fields = case Metrics of
                                           all ->
                                               Fields0;
                                           Names when is_list(Names) ->
                                               lists:filter(fun (F) ->
                                                              lists:member(
                                                                element(1, F), Names)
                                                            end, Fields0)
                                       end,
                              Labels = case LabelFormat of
                                           as_map ->
                                               LabelsAsMap;
                                           as_binary ->
                                               LabelsAsBinary
                                       end,
                              format_fields(Fields, CRef, Labels, Acc);
                          false ->
                              %% skip filtered-out and unlabeled entries
                              Acc
                      end
              end, #{}, seshat_counters_server:get_table(Group)).

format_fields(Fields, CRef, Labels, Acc) ->
    lists:foldl(
        fun ({Name0, Index, Type, Help}, Acc0) ->
                PromType = prometheus_type(Type),
                Name = with_prometheus_suffix(atom_to_binary(Name0), Type),
                InitialMetric = #{type => PromType,
                                    help => Help,
                                    values => #{}},
                MetricAcc = maps:get(Name, Acc0, InitialMetric),
                ValuesAcc = maps:get(values, MetricAcc),
                ComputedValue = case Type of
                                    {_, ratio} ->
                                        counters:get(CRef, Index) / 100;
                                    {_, time_ms} ->
                                        counters:get(CRef, Index) / 1000; % ms to s
                                    _ ->
                                        counters:get(CRef, Index) * 1.0 % ensure float
                                end,
                ValuesAcc1 = ValuesAcc#{Labels => ComputedValue},
                MetricAcc1 = MetricAcc#{values => ValuesAcc1},
                Acc0#{Name => MetricAcc1}
        end, Acc, Fields).

%% @doc Return the metadata for the fields
%% When creating a set of metrics with seshat:new/3 or seshat:new/4,
%% metadata about the metrics/counters has to be provided either
%% as a list or as a reference to a persistent term with the list.
%% The latter is recommended when a lot of similar metrics need to be stored
%% (eg. many instances of the same component emit the same set of metrics)
-spec resolve_fields_spec(fields_spec()) -> [field_spec()].
resolve_fields_spec(Fields = FieldsSpec) when is_list(FieldsSpec) ->
    Fields;
resolve_fields_spec({persistent_term, PTerm}) ->
    %% TODO error handling
    persistent_term:get(PTerm).

-spec register_counter(group(), id(), counters:counters_ref(), fields_spec(), labels()) ->
    ok.
register_counter(Group, Id, CRef, FieldsSpec, Labels) when is_map(Labels) ->
    TRef = seshat_counters_server:get_table(Group),
    Entry = #entry{
        id = Id,
        cref = CRef,
        field_spec = FieldsSpec,
        labels = Labels,
        rendered_labels = labels_to_binary(Labels)
    },
    true = ets:insert(TRef, Entry),
    ok.

-spec new_counter(group(), id(), [field_spec()], fields_spec(), labels()) ->
    counters:counters_ref().
new_counter(Group, Id, Fields, FieldsSpec, Labels) ->
    Size = length(Fields),
    ExpectedIndexes = lists:seq(1, Size),
    Indexes = lists:sort([P || {_, P, _, _} <- Fields]),
    case ExpectedIndexes == Indexes of
        true ->
            CRef = counters:new(Size, [write_concurrency]),
            ok = register_counter(Group, Id, CRef, FieldsSpec, Labels),
            CRef;
        false ->
            error(invalid_field_specification)
    end.

%% @doc Return a Prometheus-formated text (as a binary),
%% which can be directly returned by a Prometheus endpoint.
%% The returned binary has the following structure:
%% prefix_name{label1="value1",...} Value
%%
%% @param Group the name of an existing group
%% @param Names the list of metrics to return
%%
-spec prom_format(group(), string()) -> binary().
prom_format(Group, Prefix) ->
    do_prom_format(format(Group, #{metrics => all, labels => as_binary}), Prefix).

-spec prom_format(group(), string(), [atom()]) -> binary().
prom_format(Group, Prefix, Names) when is_list(Names) ->
    do_prom_format(format(Group, #{metrics => Names, labels => as_binary}), Prefix).

-spec do_prom_format(format_result(), string()) -> binary().
do_prom_format(Data, Prefix) ->
    PrefixBin = case unicode:characters_to_binary(Prefix ++ "_") of
                    P when is_tuple(P) ->
                        %% characters_to_binary errors are tuples
                        <<>>;
                    P -> P
                end,
    maps:fold(fun
                  (Name0, #{type := PromType, help := Help, values := Values}, Acc) ->
                      Name = <<PrefixBin/binary, Name0/binary>>,
                      HelpLine = <<"# HELP ", Name/binary, " ", (list_to_binary(Help))/binary>>,
                      TypeBin = atom_to_binary(PromType, utf8),
                      TypeLine = <<"# TYPE ", Name/binary, " ", TypeBin/binary>>,

                      MetricSeries = fold_values(Name, HelpLine, TypeLine, Values),
                      <<Acc/binary, MetricSeries/binary>>
              end, <<"">>, Data).

fold_values(Name, Help, Type, Values) when
      is_binary(Name),
      is_binary(Help),
      is_binary(Type),
      is_map(Values) ->
    maps:fold(fun
                  (Labels, Value, SeriesAcc) when is_binary(Labels) ->
                      LabelsBin = <<"{", Labels/binary, "} ">>,
                      FormattedValue = float_to_binary(Value, [{decimals, 3}, compact]),
                      Line = <<Name/binary, LabelsBin/binary, FormattedValue/binary>>,
                      <<SeriesAcc/binary, Line/binary, "\n">>
              end, <<Help/binary, "\n", Type/binary, "\n">>, Values).

-spec prometheus_type(metric_type()) -> prometheus_type().
prometheus_type(counter) -> counter;
prometheus_type(gauge) -> gauge;
prometheus_type({counter, _}) -> counter;
prometheus_type({gauge, _}) -> gauge.

-spec with_prometheus_suffix(binary(), metric_type()) -> binary().
with_prometheus_suffix(Name, {_, ratio}) -> <<Name/binary, "_ratio">>;
with_prometheus_suffix(Name, {_, time_ms}) -> <<Name/binary, "_seconds">>;
with_prometheus_suffix(Name, {_, time_s}) -> <<Name/binary, "_seconds">>;
with_prometheus_suffix(Name, _) -> Name.

label_value_to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
label_value_to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
label_value_to_binary(Value) when is_binary(Value) ->
    Value.

labels_to_binary(Labels) when is_map(Labels) ->
    LabelsBin0 = maps:fold(
                   fun (K, V, LabelsAcc) ->
                           LabelKey = atom_to_binary(K, utf8),
                           LabelValue = label_value_to_binary(V),
                           <<LabelsAcc/binary, LabelKey/binary, "=\"", LabelValue/binary, "\",">>
                   end, <<"">>, Labels),
    case LabelsBin0 of
        <<>> -> <<>>;
        _    ->
            binary:part(LabelsBin0, 0, byte_size(LabelsBin0) - 1)
    end.
