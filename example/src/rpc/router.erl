%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for router.
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2018-10-23T01:03:03+00:00 and should not be modified manually

-module(router).

-export([decode/1, encode/2]).

-spec decode(binary()) -> {RouterCmd, DecodeMsg} when
    RouterCmd::binary(),
    DecodeMsg::tuple().
decode(Cmd) -> throw({error, cmd_not_found, Cmd}).

-spec encode(binary(), lists()) -> binary().
encode(Cmd, _Values) ->
    throw({error, cmd_not_found, Cmd}).
