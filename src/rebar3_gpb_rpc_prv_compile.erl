-module(rebar3_gpb_rpc_prv_compile).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, gen).
-define(NAMESPACE, gpb_rpc).
-define(DEPS, [{default, app_discovery}]).

-include_lib("gpb/include/gpb.hrl").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {namespace, ?NAMESPACE},
        {module, ?MODULE},            % The module implementation of the task
        {bare, true},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 gpb_rpc gen"}, % How to use the plugin
        {opts, []},                   % list of options understood by the plugin
        {short_desc, "Generates gpb rpc"},
        {desc, "Generates gpb rpc"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Config = rebar_state:opts(State),
    RpcConfig = rebar_opts:get(Config, gpb_rpc_opts, []),
    GpbOpts = rebar_opts:get(Config, gpb_opts, []),
    {Options, _} = rebar_state:command_parsed_args(State),
    ProtoDir = proplists:get_value(protos, Options, proplists:get_value(protos, RpcConfig, "priv/protos")),
    rebar_api:warn("rpc config ~p ~p ~n", [RpcConfig, GpbOpts]),
    RouterMod = get_router_module(RpcConfig, GpbOpts),
    [begin
         GpbModule = preload_pb(Filename, GpbOpts),
         try
             gen_rpc_router_mod(RouterMod, GpbModule, Options, RpcConfig, State)
         catch E:R ->
             rebar_utils:abort("Failed to generate Router ~p ~p: ~p ~p ~nPlease run rebar3 protobuf compile first!~n",
                 [GpbModule, E, R, erlang:get_stacktrace()])
         end
     end || Filename <- filelib:wildcard(filename:join(ProtoDir, "*.proto"))],
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

get_router_module(RpcConfig, GpbOpts) ->
    RouterMod = proplists:get_value(router_module, RpcConfig, "router"),
    ModuleNameSuffix = proplists:get_value(module_name_suffix, GpbOpts, "_pb"),
    ModuleNamePrefix = proplists:get_value(module_name_prefix, GpbOpts, ""),
    list_to_atom(ModuleNamePrefix ++ RouterMod ++ ModuleNameSuffix).

preload_pb(Filename, Options) ->
    OutDir = proplists:get_value(o, Options, "src/protos"),
    ModuleNameSuffix = proplists:get_value(module_name_suffix, Options, "_pb"),
    ModuleNamePrefix = proplists:get_value(module_name_prefix, Options, ""),
    CompiledPB = filename:join(OutDir, ModuleNamePrefix ++ filename:basename(Filename, ".proto") ++ ModuleNameSuffix ++ ".erl"),
    rebar_log:log(info, "Loading ~s", [CompiledPB]),
    GpbIncludeDir = filename:join(code:lib_dir(gpb), "include"),
    case compile:file(CompiledPB,
        [binary, {i, GpbIncludeDir}, {i, "./include/"}, return_errors]) of
        {ok, Module, Compiled} ->
            {module, _} = code:load_binary(Module, CompiledPB, Compiled),
            Module;
        {ok, Module, Compiled, Warnings} ->
            [begin
                 rebar_api:warn("Warning building ~s~n", [File]),
                 [rebar_api:warn("        ~p: ~s", [Line, M:format_error(E)]) || {Line, M, E} <- Es]
             end || {File, Es} <- Warnings],
            {module, _} = code:load_binary(Module, CompiledPB, Compiled),
            Module;
        {error, Errors, Warnings} ->
            rebar_utils:abort("Failed to loading ~p ~p: ~p ~p ~nPlease run rebar3 protobuf compile first!~n",
                [CompiledPB, Errors, Warnings, erlang:get_stacktrace()])
    end.
gen_rpc_router_mod(GpbModule, GpbModule, _Options, _RpcConfig, _State) -> [];
gen_rpc_router_mod(RouterMod, GpbModule, Options, RpcConfig, State) ->
    RouterEnum = proplists:get_value(router_enum, RpcConfig, "router_service"),
    RouterPackage = atom_to_list(RouterMod:get_package_name()),
    Commands = lists:flatten(
        [begin
             {{_, NameAtom}, Methods} = GpbModule:get_service_def(S),
             Name = atom_to_list(NameAtom),
             [PackageName, Module] = string:tokens(Name, "."),
             EnumValFunc1 = list_to_atom("enum_value_by_symbol_" ++ RouterPackage ++ "." ++ RouterEnum),
             Cmd1 = RouterMod:EnumValFunc1(list_to_atom(PackageName)),
             EnumValFun2 = list_to_atom("enum_value_by_symbol_" ++ PackageName ++ "." ++ RouterEnum),
             Cmd2 = GpbModule:EnumValFun2(list_to_atom(Module)),
             rebar_log:log(debug, "GpbModule: ~p~n", [{GpbModule, Name, Methods, Cmd1, Cmd2, EnumValFunc1, EnumValFun2}]),
             [begin
                  {rpc, MethodName, Input, Output, _InputStream, _OutputStream, Opts} = Method,
                  MethodNameStr = atom_to_list(MethodName),
                  Cmd3 = proplists:get_value(seq, Opts),
                  Key = {Cmd1, Cmd2, Cmd3},
                  OutputType = find_type(GpbModule, Output),
                  {Key,
                      [
                          {method, list_snake_case(MethodNameStr)},
                          {unmodified_service_name, Module},
                          {unmodified_method, MethodNameStr},
                          {pb_module, atom_to_list(GpbModule)},
                          {input, Input},
                          {output, Output},
                          {output_base_type, OutputType =/= base_type},
                          {output_simple_array_type, OutputType =/= simple_array_type},
                          {output_composite_array_type, OutputType =:= simple_array_type orelse OutputType =:= base_type},
                          {output_composite_array, OutputType},
                          {cmd1, Cmd1},
                          {cmd2, Cmd2},
                          {cmd3, Cmd3}
                      ]}
              end || Method <- Methods]
         end || S <- GpbModule:get_service_names()]),
    is_well_formed(Commands),
    Service =
        [
            {out_dir, proplists:get_value(o_erl, RpcConfig, "src")},
            {router_module, proplists:get_value(router_module, RpcConfig, "router")},
            {commands, [List || {_, List} <- Commands]}
        ],
    rebar_log:log(debug, "services: ~p", [Service]),
    Force = proplists:get_value(force, Options, true),
    rebar_templater:new("gpb_rpc", Service, Force, State).

is_well_formed(Commands) ->
    SortCommands = lists:ukeysort(1, Commands),
    case length(SortCommands) =:= length(Commands) of
        true -> ok;
        false -> rebar_utils:abort("Commands is repeated ~p ~n", [Commands -- SortCommands])
    end.

list_snake_case(NameString) ->
    Snaked = lists:foldl(fun(RE, Snaking) ->
        re:replace(Snaking, RE, "\\1_\\2", [{return, list}, global])
                         end, NameString, [%% uppercase followed by lowercase
        "(.)([A-Z][a-z]+)",
        %% any consecutive digits
        "(.)([0-9]+)",
        %% uppercase with lowercase
        %% or digit before it
        "([a-z0-9])([A-Z])"]),
    Snaked1 = string:replace(Snaked, ".", "_", all),
    Snaked2 = string:replace(Snaked1, "__", "_", all),
    string:to_lower(unicode:characters_to_list(Snaked2)).

find_type(GpbMod, Rec) ->
    [#field{type = Type, occurrence = Occurrence} | _] = GpbMod:fetch_msg_def(Rec),
    case Type of
        {msg, SubRec} -> SubRec; %% composite_array
        _ when Occurrence =:= 'repeated' -> simple_array_type;
        _ -> base_type
    end.
