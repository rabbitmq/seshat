%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2023 Broadcom. All Rights Reserved. The term Broadcom refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(seshat).

-export([new_group/1,
         delete_group/1,
         new/3,
         new/4,
         fetch/2,
         overview/1,
         overview/2,
         counters/2,
         counters/3,
         delete/2,
         format/1,
         format/2,
         format_one/2
        ]).

-type group() :: term().
-opaque group_ref() :: ets:tid().
-type name() :: term().

-type field_spec() :: {Name :: atom(), Index :: pos_integer(),
                       Type :: counter | gauge | ratio, Help :: string()}.

-type fields_spec() :: [field_spec()] | {persistent_term, term()}.

-type format_result() :: #{Name :: atom() =>
                           #{type => counter | gauge,
                             help => string(),
                             values => #{labels() => number()}}}.

-type label_name() :: atom() | unicode:chardata().
-type label_value() :: atom() | unicode:chardata().

-type labels() :: #{label_name() => label_value()}.

-export_type([name/0,
              group/0,
              group_ref/0,
              field_spec/0,
              fields_spec/0,
              labels/0]).

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
%% @param FieldSpec metadata for the values stored in the counter
%% @returns a reference to the counter
-spec new(group(), name(), fields_spec()) ->
    counters:counters_ref().
new(Group, Id, FieldSpec)  ->
    new(Group, Id, FieldSpec, #{}).

%% @doc Create a new set of metrics.
%% A set of metrics is stored in a counter (@see counters)
%%
%% @param Group the name of an existing group
%% @param Id the id of an object these metrics are assosiated with
%% @param FieldSpec metadata for the values stored in the counter
%% @param Labels key-value pairs describing the object
%% @returns a reference to the counter
-spec new(group(), name(), fields_spec(), labels()) ->
    counters:counters_ref().
new(Group, Id, Fields = FieldSpec, Labels) when is_list(Fields) ->
    new_counter(Group, Id, Fields, FieldSpec, Labels);
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
-spec fetch(group(), name()) -> undefined | counters:counters_ref().
fetch(Group, Id) ->
    TRef = seshat_counters_server:get_table(Group),
    try
        ets:lookup_element(TRef, Id, 2)
    catch
        error:badarg ->
            undefined
    end.

%% @doc Delete a metric set
%%
%% @param Group the name of an existing group
%% @param Id the id of an existing object
%% @returns 'ok'
-spec delete(group(), name()) -> ok.
delete(Group, Id) ->
    TRef = seshat_counters_server:get_table(Group),
    true = ets:delete(TRef, Id),
    ok.

%% @doc Return a map with all metrics of all objects in the group
%% The returned map has the following structure:
%% #{Id => #{Name => Value}}
%%
%% @param Group the name of an existing group
-spec overview(group()) ->
    #{name() => #{atom() => integer()}}.
overview(Group) ->
    ets:foldl(
      fun({Id, CRef, FieldSpec, _Labels}, Acc) ->
              Fields = resolve_fields_spec(FieldSpec),
              Counters = lists:foldl(
                           fun ({Name, Index, _Type, _Help}, Acc0) ->
                                   Acc0#{Name => counters:get(CRef, Index)}
                           end, #{}, Fields),
              Acc#{Id => Counters}
      end, #{}, seshat_counters_server:get_table(Group)).

%% @doc Return a map with all metrics for the object
%% The returned map has the following structure:
%% #{Name => Value}
%%
%% @param Group the name of an existing group
%% @param Id the name of an existing object
%% @deprecated Use counters/2 instead
-spec overview(group(), name()) ->
    #{atom() => integer()} | undefined.
overview(Group, Id) ->
    counters(Group, Id).

%% @doc Return a map with all metrics for the object
%% The returned map has the following structure:
%% #{Name => Value}
%%
%% @param Group the name of an existing group
%% @param Id the name of an existing object
-spec counters(group(), name()) ->
    #{atom() => integer()} | undefined.
counters(Group, Id) ->
    case ets:lookup(seshat_counters_server:get_table(Group), Id) of
        [{Id, CRef, FieldSpec, _Labels}] ->
            Fields = resolve_fields_spec(FieldSpec),
            lists:foldl(fun ({Key, Index, _Type, _Help}, Acc0) ->
                                Acc0#{Key => counters:get(CRef, Index)}
                        end, #{}, Fields);
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
-spec counters(group(), name(), [atom()]) ->
    #{atom() => integer()} | undefined.
counters(Group, Id, Names) ->
    case ets:lookup(seshat_counters_server:get_table(Group), Id) of
        [{Id, CRef, FieldSpec, _Labels}] ->
            Fields = resolve_fields_spec(FieldSpec),
            lists:foldl(fun ({Name, Index, _Type, _Help}, Acc0) ->
                                case lists:member(Name, Names) of
                                    true ->
                                        Acc0#{Name => counters:get(CRef, Index)};
                                    false ->
                                        Acc0
                                end
                        end, #{}, Fields);
        _ ->
            undefined
    end.

%% @doc Return a map with all metrics for all objects in the group
%% The returned map has the following structure:
%% #{Name => #{Labels => Value}}
%% This structure is similar to what Prometheus expects,
%% with label sets associated with metric values.
%%
%% @param Group the name of an existing group
-spec format(group()) -> format_result().
format(Group) ->
    ets:foldl(fun
                  ({_Name, _CRef, _FieldSpec, Labels}, Acc) when map_size(Labels) == 0 ->
                      Acc;
                  ({_Name, CRef, FieldsSpec, Labels}, Acc) ->
                      Fields = resolve_fields_spec(FieldsSpec),
                      format_fields(Fields, CRef, Labels, Acc)
              end, #{}, seshat_counters_server:get_table(Group)).

%% @doc Return a map with selected metrics for all objects
%% The returned map has the following structure:
%% #{Name => #{Labels => Value}}
%% This structure is similar to what Prometheus expects,
%% with label sets associated with metric values.
%%
%% @param Group the name of an existing group
%% @param Names the list of metrics to return
-spec format(group(), [atom()]) -> format_result().
format(Group, Names) when is_list(Names) ->
    ets:foldl(fun
                  ({_Name, _CRef, _FieldSpec, Labels}, Acc) when map_size(Labels) == 0 ->
                      Acc;
                  ({_Name, CRef, FieldSpec, Labels}, Acc) ->
                      Fields0 = resolve_fields_spec(FieldSpec),
                      Fields = lists:filter(fun (F) -> lists:member(element(1, F), Names) end, Fields0),
                      format_fields(Fields, CRef, Labels, Acc)
      end, #{}, seshat_counters_server:get_table(Group)).

%% @doc Return a map with all metrics for the selected object
%% The returned map has the following structure:
%% #{Name => #{Labels => Value}}
%% This structure is similar to what Prometheus expects,
%% with label sets associated with metric values.
%%
%% @param Group the name of an existing group
%% @param Names the list of metrics to return
-spec format_one(group(), name()) -> format_result().
format_one(Group, Id) ->
    case ets:lookup(seshat_counters_server:get_table(Group), Id) of
        [{Id, CRef, FieldSpec, Labels}] when map_size(Labels) > 0 ->
            Fields = resolve_fields_spec(FieldSpec),
            format_fields(Fields, CRef, Labels, #{});
        _ ->
            #{}
    end.

%% internal

%% @doc Return the metadata for the fields
%% When creating a set of metrics with seshat:new/3 or seshat:new/4,
%% metadata about the metrics/counters has to be provided either
%% as a list or as a reference to a persistent term with the list.
%% The latter is recommended when a lot of similar metrics need to be stored
%% (eg. many instances of the same component emit the same set of metrics)
-spec resolve_fields_spec(fields_spec()) -> [field_spec()].
resolve_fields_spec(Fields = FieldSpec) when is_list(FieldSpec) ->
    Fields;
resolve_fields_spec({persistent_term, PTerm}) ->
    %% TODO error handling
    persistent_term:get(PTerm).

-spec register_counter(group(), name(), counters:counters_ref(), fields_spec(), labels()) ->
    ok.
register_counter(Group, Id, CRef, FieldSpec, Labels) ->
    TRef = seshat_counters_server:get_table(Group),
    true = ets:insert(TRef, {Id, CRef, FieldSpec, Labels}),
    ok.

-spec new_counter(group(), name(), [field_spec()], fields_spec(), labels()) ->
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

format_fields(Fields, CRef, Labels, Acc) ->
    lists:foldl(
        fun ({Name, Index, Type, Help}, Acc0) ->
                ComputedType = case Type of
                                    ratio -> gauge;
                                    Other -> Other
                                end,
                InitialMetric = #{type => ComputedType,
                                    help => Help,
                                    values => #{}},
                MetricAcc = maps:get(Name, Acc0, InitialMetric),
                ValuesAcc = maps:get(values, MetricAcc),
                ComputedValue = case Type of
                                    ratio -> counters:get(CRef, Index) / 100;
                                    _ -> counters:get(CRef, Index)
                                end,
                ValuesAcc1 = ValuesAcc#{Labels => ComputedValue},
                MetricAcc1 = MetricAcc#{values => ValuesAcc1},
                Acc0#{Name => MetricAcc1}
        end, Acc, Fields).
