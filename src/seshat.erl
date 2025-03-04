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

-type field_spec() :: {Name :: atom(), Position :: pos_integer(),
                       Type :: counter | gauge | ratio, Description :: string()}.

-type fields_spec() :: [field_spec()] | {persistent_term, term()}.

-type format_result() :: #{FieldName :: atom() =>
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

-spec new_group(group()) -> group_ref().
new_group(Group) ->
    seshat_counters_server:create_table(Group).

-spec delete_group(group()) -> ok.
delete_group(Group) ->
    seshat_counters_server:delete_table(Group).

-spec new(group(), name(), fields_spec()) ->
    counters:counters_ref().

new(Group, Name, Fields) when is_list(Fields) ->
    new(Group, Name, Fields, #{});
new(Group, Name, {persistent_term, _PTerm} = FieldsSpec) ->
    new(Group, Name, FieldsSpec, #{}).

-spec new(group(), name(), fields_spec(), labels()) ->
    counters:counters_ref().

new(Group, Name, Fields, Labels) when is_list(Fields) ->
    new_counter(Group, Name, Fields, Fields, Labels);
new(Group, Name, {persistent_term, PTerm} = FieldsSpec, Labels) ->
    case persistent_term:get(PTerm, undefined) of
        undefined ->
            error({non_existent_fields_spec, FieldsSpec});
        Fields ->
            new_counter(Group, Name, Fields, FieldsSpec, Labels)
    end.

%% fetch/2 is NOT meant to be called for every counter update.
%% Instead, for higher performance, the consuming application should store the returned counters_ref
%% in a stateful Erlang module or in persistent_term (see persistent:term_put/2).
-spec fetch(group(), name()) -> undefined | counters:counters_ref().
fetch(Group, Name) ->
    TRef = seshat_counters_server:get_table(Group),
    try
        ets:lookup_element(TRef, Name, 2)
    catch
        error:badarg ->
            undefined
    end.

-spec delete(group(), name()) -> ok.
delete(Group, Name) ->
    TRef = seshat_counters_server:get_table(Group),
    true = ets:delete(TRef, Name),
    ok.

%%% Use counters/2
-spec overview(group()) ->
    #{name() => #{atom() => integer()}}.
overview(Group) ->
    ets:foldl(
      fun({Name, Ref, Fields0, _Labels}, Acc) ->
              Fields = resolve_fields(Fields0),
              Counters = lists:foldl(
                           fun ({Key, Index, _Type, _Description}, Acc0) ->
                                   Acc0#{Key => counters:get(Ref, Index)}
                           end, #{}, Fields),
              Acc#{Name => Counters}
      end, #{}, seshat_counters_server:get_table(Group)).

-spec overview(group(), name()) ->
    #{atom() => integer()} | undefined.
overview(Group, Name) ->
    counters(Group, Name).

-spec counters(group(), name()) ->
    #{atom() => integer()} | undefined.
counters(Group, Name) ->
    case ets:lookup(seshat_counters_server:get_table(Group), Name) of
        [{Name, Ref, Fields0, _Labels}] ->
            Fields = resolve_fields(Fields0),
            lists:foldl(fun ({Key, Index, _Type, _Description}, Acc0) ->
                                Acc0#{Key => counters:get(Ref, Index)}
                        end, #{}, Fields);
        _ ->
            undefined
    end.

-spec counters(group(), name(), [atom()]) ->
    #{atom() => integer()} | undefined.
counters(Group, Name, FieldNames) ->
    case ets:lookup(seshat_counters_server:get_table(Group), Name) of
        [{Name, Ref, Fields0, _Labels}] ->
            Fields = resolve_fields(Fields0),
            lists:foldl(fun ({Key, Index, _Type, _Description}, Acc0) ->
                                case lists:member(Key, FieldNames) of
                                    true ->
                                        Acc0#{Key => counters:get(Ref, Index)};
                                    false ->
                                        Acc0
                                end
                        end, #{}, Fields);
        _ ->
            undefined
    end.

-spec format(group()) -> format_result().
format(Group) ->
    ets:foldl(fun
                  ({_Name, _Ref, _Fields0, Labels}, Acc) when map_size(Labels) == 0 ->
                      Acc;
                  ({_Name, Ref, Fields0, Labels}, Acc) ->
                      Fields = resolve_fields(Fields0),
                      format_fields(Fields, Ref, Labels, Acc)
              end, #{}, seshat_counters_server:get_table(Group)).

-spec format(group(), [atom()]) -> format_result().
format(Group, FieldNames) ->
    ets:foldl(fun
                  ({_Id, _Ref, _Fields0, Labels}, Acc) when map_size(Labels) == 0 ->
                      Acc;
                  ({_Id, Ref, Fields0, Labels}, Acc) ->
                      Fields1 = resolve_fields(Fields0),
                      Fields = lists:filter(fun (F) -> lists:member(element(1, F), FieldNames) end, Fields1),
                      format_fields(Fields, Ref, Labels, Acc)
      end, #{}, seshat_counters_server:get_table(Group)).

-spec format_one(group(), name()) -> format_result().
format_one(Group, Name) ->
    case ets:lookup(seshat_counters_server:get_table(Group), Name) of
        [{Name, Ref, Fields0, Labels}] when map_size(Labels) > 0 ->
            Fields = resolve_fields(Fields0),
            format_fields(Fields, Ref, Labels, #{});
        _ ->
            #{}
    end.

%% internal

resolve_fields(Fields) when is_list(Fields) ->
    Fields;
resolve_fields({persistent_term, PTerm}) ->
    %% TODO error handling
    persistent_term:get(PTerm).

register_counter(Group, Name, Ref, Fields, Labels) ->
    TRef = seshat_counters_server:get_table(Group),
    true = ets:insert(TRef, {Name, Ref, Fields, Labels}),
    ok.

new_counter(Group, Name, Fields, FieldsSpec, Labels) ->
    Size = length(Fields),
    ExpectedPositions = lists:seq(1, Size),
    Positions = lists:sort([P || {_, P, _, _} <- Fields]),
    case ExpectedPositions == Positions of
        true ->
            CRef = counters:new(Size, [write_concurrency]),
            ok = register_counter(Group, Name, CRef, FieldsSpec, Labels),
            CRef;
        false ->
            error(invalid_field_specification)
    end.

format_fields(Fields, Ref, Labels, Acc) ->
    lists:foldl(
      fun ({MetricName, Index, ratio, Help}, Acc0) ->
              InitialMetric = #{type => gauge,
                                help => Help,
                                values => #{}},
              Metric = maps:get(MetricName, Acc0, InitialMetric),
              Values = maps:get(values, Metric),
              Counter = counters:get(Ref, Index) / 100,
              Values1 = Values#{Labels => Counter},
              Metric1 = Metric#{values => Values1},
              Acc0#{MetricName => Metric1};
        ({MetricName, Index, Type, Help}, Acc0) ->
              InitialMetric = #{type => Type,
                                help => Help,
                                values => #{}},
              Metric = maps:get(MetricName, Acc0, InitialMetric),
              Values = maps:get(values, Metric),
              Counter = counters:get(Ref, Index),
              Values1 = Values#{Labels => Counter},
              Metric1 = Metric#{values => Values1},
              Acc0#{MetricName => Metric1}
      end, Acc, Fields).
