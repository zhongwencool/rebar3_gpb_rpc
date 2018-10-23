%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for router.
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on {{datetime}} and should not be modified manually

-module({{router_module}}).

-export([decode/1, encode/2]).

-spec decode(binary()) -> {RouterCmd, DecodeMsg} when
    RouterCmd :: binary(),
    DecodeMsg :: tuple().
{{#commands}}
decode(<<{{cmd1}}:10,{{cmd2}}:10,{{cmd3}}:12,Bin/binary>>) -> {<<{{cmd1}}:10,{{cmd2}}:10,{{cmd3}}:12>>, '{{pb_module}}':decode_msg(Bin, '{{input}}', [])};
{{/commands}}
decode(Cmd) -> throw({error, cmd_not_found, Cmd}).

-spec encode(binary(), list()) -> binary().
{{#commands}}
encode(<<{{cmd1}}:10, {{cmd2}}:10, {{cmd3}}:12>>, Values) -> %% {{unmodified_service_name}}({{input}}) -> {{output}}
    {{^output_base_type}}Rec = list_to_tuple(['{{output}}' | Values]),
    '{{pb_module}}':encode_msg(Rec, '{{output}}', []);{{/output_base_type}}{{^output_simple_array_type}}Rec = {'{{output}}', Values},
    '{{pb_module}}':encode_msg(Rec, '{{output}}', []);{{/output_simple_array_type}}{{^output_composite_array_type}}Rec = {'{{output}}', [begin list_to_tuple(['{{output_composite_array}}' | V]) end||V <- Values]},
    '{{pb_module}}':encode_msg(Rec, '{{output}}', []);{{/output_composite_array_type}}
{{/commands}}
encode(Cmd, _Values) ->
    throw({error, cmd_not_found, Cmd}).
