
A rebar3 plugin for generating a behaviour gpb rpc

Build
-----

```
$ rebar3 protobuf compile
$ rebar3 gpb_rpc gen
```

Use
---

Add the plugin to your rebar config:

```erlang
{gpb_rpc_opts, [
    {router_module, "rpc_router"}, %% default is router
    {router_enum, "router_service"}, %% default is router_service
    {o_erl, "src/rpc"}
]}.

{gpb_opts, [
    {recursive, true},
    {i, "priv/protos"},
    {ipath, "_build/default/plugins/gpb/priv/proto3/"},
    use_packages,
    {module_name_suffix, "_pb"},
    {o_erl, "src/protos"},
    {o_hrl, "include"},
    {strings_as_binaries, true},
    type_specs]
}.

{plugins, [
    rebar3_gpb_plugin,
    {rebar3_gpb_rpc, {git, "https://github.com/zhongwencool/rebar3_gpb_rpc", {branch, "master"}}}
]}.
```
