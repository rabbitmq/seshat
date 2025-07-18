PROJECT = seshat
PROJECT_DESCRIPTION = Metrics library built for RabbitMQ, with a Prometheus exposition format built-in
PROJECT_VERSION = 0.6.1

dep_cth_styledout = git https://github.com/rabbitmq/cth_styledout.git master
TEST_DEPS = cth_styledout
CT_OPTS = -ct_hooks cth_styledout

define PROJECT_ENV
[
]
endef

LOCAL_DEPS = sasl crypto

# PLT_APPS += eunit syntax_tools erts kernel stdlib common_test inets ssh ssl meck gen_batch_server inet_tcp_proxy

DIALYZER_OPTS += --src -r test
include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)
