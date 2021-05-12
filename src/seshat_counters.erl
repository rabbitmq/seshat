%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(seshat_counters).

-export([new_group/1,
         delete_group/1,
         new/3,
         fetch/2,
         overview/1,
         delete/2,
         prometheus_format/1,
         gc/2
        ]).

-type group() :: term().
-type name() :: term().

new_group(Group) ->
    seshat_counters_server:create_table(Group).

delete_group(Group) ->
    seshat_counters_server:delete_table(Group).

-spec new(group(), name(), [atom()]) -> counters:counters_ref().
new(Group, Name, Fields) when is_list(Fields) ->
    Size = length(Fields),
    CRef = counters:new(Size, []),
    ok = register_counter(Group, Name, CRef, Fields),
    CRef.

-spec fetch(group(), name()) -> undefined | counters:counters_ref().
fetch(Group, Name) ->
    TRef = seshat_counters_server:get_table(Group),
    case ets:lookup(TRef, Name) of
        [{Name, Ref, _}] ->
            Ref;
        _ ->
            undefined
    end.

-spec delete(group(), name()) -> ok.
delete(Group, Name) ->
    TRef = seshat_counters_server:get_table(Group),
    true = ets:delete(TRef, Name),
    ok.

-spec overview(group()) -> #{name() => #{atom() => non_neg_integer()}}.
overview(Group) ->
    ets:foldl(fun({Name, Ref, Fields}, Acc) ->
                      Counters = lists:foldl(
                                   fun ({Key, Index, _Type, _Description}, Acc0) ->
                                           Acc0#{Key => counters:get(Ref, Index)}
                                   end,
                                   #{},
                                   Fields
                                  ),
                      Acc#{Name => Counters}
              end,
              #{}, seshat_counters_server:get_table(Group)).

prometheus_format(Group) ->
    ets:foldl(fun({Name, Ref, Fields}, Acc) ->
                      Counters = lists:foldl(
                                   fun ({Key, Index, Type, Description}, Acc0) ->
                                           [{Key, counters:get(Ref, Index), Type, Description}
                                           | Acc0]
                                   end,
                                   [],
                                   Fields
                                  ),
                      Acc#{Name => Counters}
              end,
              #{}, seshat_counters_server:get_table(Group)).

%% TODO maybe seshat should be responsible for all gc timers?
gc(Group, Fun) ->
    Table = seshat_counters_server:get_table(Group),
    ets:foldl(fun({Name, _Ref, _Fields}, none) ->
                      case Fun(Name) of
                          true ->
                              ets:delete(Table, Name);
                          false ->
                              none
                      end
              end, none, Table).

%% internal

register_counter(Group, Name, Ref, Fields) ->
    TRef = seshat_counters_server:get_table(Group),
    true = ets:insert(TRef, {Name, Ref, Fields}),
    ok.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.
