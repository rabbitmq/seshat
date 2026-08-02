%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2025 Broadcom. All Rights Reserved. The term Broadcom refers to Broadcom Inc. and/or its subsidiaries.
%%

-type group() :: term().
-opaque group_ref() :: ets:tid().
-type id() :: term().

-type metric_unit() :: ratio | time_s | time_ms.
-type metric_type() :: counter | gauge | {counter | gauge, metric_unit()}.

%% Histograms are rendered directly by seshat_histogram rather than going
%% through format_result(), so 'histogram' does not appear here.
-type prometheus_type() :: counter | gauge.

-type field_spec() :: {Name :: atom(), Index :: pos_integer(),
                       Type :: metric_type(), Help :: string()}.

-type fields_spec() :: [field_spec()] | {persistent_term, term()}.

-type format_options() :: #{metrics => all | [atom()],
                            labels => as_map | as_binary,
                            filter_fun => function()}.

-type format_result() :: #{Name :: binary() =>
                           #{type => prometheus_type(),
                             help => string(),
                             values => #{labels() => float()}}}.

-type label_name() :: atom().
-type label_value() :: atom() | unicode:chardata().

-type labels_map() :: #{label_name() => label_value()}.
-type labels() :: labels_map() | binary().

-type upper_bound() :: non_neg_integer() | infinity.
-type bucket_spec() :: [upper_bound()].

-export_type([
              id/0,
              group/0,
              group_ref/0,
              field_spec/0,
              fields_spec/0,
              labels/0,
              labels_map/0,
              upper_bound/0,
              bucket_spec/0
             ]).

-record(entry,
        {id :: term(),
         cref :: counters:counters_ref(),
         field_spec :: fields_spec(),
         labels :: labels_map(),
         rendered_labels :: binary()}).

%% Histogram entries live in the same per-group ETS table as #entry{}
%% records: both have their id in position 2, so the table's keypos is
%% shared. Every read of the group table must therefore match on the
%% record tag rather than assuming a field position.
-record(histogram_entry,
        {id :: term(),
         cref :: counters:counters_ref(),
         bounds :: tuple(),
         sum_pos :: pos_integer(),
         name :: atom(),
         help :: string(),
         labels :: labels_map(),
         rendered_labels :: binary()}).
