load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files(name = "all_beam_files"):
    filegroup(
        name = "beam_files",
        srcs = ["ebin/seshat.beam", "ebin/seshat_app.beam", "ebin/seshat_counters_server.beam", "ebin/seshat_sup.beam"],
    )
    erlang_bytecode(
        name = "ebin_seshat_app_beam",
        srcs = ["src/seshat_app.erl"],
        outs = ["ebin/seshat_app.beam"],
        app_name = "seshat",
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_seshat_beam",
        srcs = ["src/seshat.erl"],
        outs = ["ebin/seshat.beam"],
        app_name = "seshat",
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_seshat_counters_server_beam",
        srcs = ["src/seshat_counters_server.erl"],
        outs = ["ebin/seshat_counters_server.beam"],
        app_name = "seshat",
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_seshat_sup_beam",
        srcs = ["src/seshat_sup.erl"],
        outs = ["ebin/seshat_sup.beam"],
        app_name = "seshat",
        erlc_opts = "//:erlc_opts",
    )

def all_test_beam_files(name = "all_test_beam_files"):
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/seshat.beam", "test/seshat_app.beam", "test/seshat_counters_server.beam", "test/seshat_sup.beam"],
    )
    erlang_bytecode(
        name = "test_seshat_app_beam",
        testonly = True,
        srcs = ["src/seshat_app.erl"],
        outs = ["test/seshat_app.beam"],
        app_name = "seshat",
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_seshat_beam",
        testonly = True,
        srcs = ["src/seshat.erl"],
        outs = ["test/seshat.beam"],
        app_name = "seshat",
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_seshat_counters_server_beam",
        testonly = True,
        srcs = ["src/seshat_counters_server.erl"],
        outs = ["test/seshat_counters_server.beam"],
        app_name = "seshat",
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_seshat_sup_beam",
        testonly = True,
        srcs = ["src/seshat_sup.erl"],
        outs = ["test/seshat_sup.beam"],
        app_name = "seshat",
        erlc_opts = "//:test_erlc_opts",
    )

def test_suite_beam_files(name = "test_suite_beam_files"):
    erlang_bytecode(
        name = "test_seshat_counters_server_test_beam",
        testonly = True,
        srcs = ["test/seshat_counters_server_test.erl"],
        outs = ["test/seshat_counters_server_test.beam"],
        app_name = "seshat",
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_seshat_test_beam",
        testonly = True,
        srcs = ["test/seshat_test.erl"],
        outs = ["test/seshat_test.beam"],
        app_name = "seshat",
        erlc_opts = "//:test_erlc_opts",
    )

def all_srcs(name = "all_srcs"):
    filegroup(
        name = "all_srcs",
        srcs = [":public_and_private_hdrs", ":srcs"],
    )

    filegroup(
        name = "priv",
    )
    filegroup(
        name = "private_hdrs",
    )
    filegroup(
        name = "public_and_private_hdrs",
        srcs = [":private_hdrs", ":public_hdrs"],
    )
    filegroup(
        name = "public_hdrs",
    )
    filegroup(
        name = "srcs",
        srcs = ["src/seshat.app.src", "src/seshat.erl", "src/seshat_app.erl", "src/seshat_counters_server.erl", "src/seshat_sup.erl"],
    )
    filegroup(
        name = "license_files",
        srcs = [
            "LICENSE",
            "LICENSE-APACHE2",
            "LICENSE-MPL-RabbitMQ",
        ],
    )
