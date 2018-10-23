%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for router.
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2018-10-23T06:29:44+00:00 and should not be modified manually

-module(rpc_router).

-export([decode/1, encode/2]).

-spec decode(binary()) -> {RouterCmd, DecodeMsg} when
    RouterCmd :: binary(),
    DecodeMsg :: tuple().
decode(<<1:10,1:10,1:12,Bin/binary>>) -> {<<1:10,1:10,1:12>>, 'user_pb':decode_msg(Bin, 'User.Name', [])};
decode(<<1:10,1:10,2:12,Bin/binary>>) -> {<<1:10,1:10,2:12>>, 'user_pb':decode_msg(Bin, 'User.Name', [])};
decode(<<1:10,1:10,3:12,Bin/binary>>) -> {<<1:10,1:10,3:12>>, 'user_pb':decode_msg(Bin, 'User.RangeRequest', [])};
decode(<<1:10,2:10,1:12,Bin/binary>>) -> {<<1:10,2:10,1:12>>, 'user_pb':decode_msg(Bin, 'User.Name', [])};
decode(Cmd) -> throw({error, cmd_not_found, Cmd}).

-spec encode(binary(), lists()) -> binary().
encode(<<1:10, 1:10, 1:12>>, Values) -> %% Search(User.Name) -> User.Person
    Rec = list_to_tuple(['User.Person' | Values]),
    'user_pb':encode_msg(Rec, 'User.Person', []);
encode(<<1:10, 1:10, 2:12>>, Values) -> %% Search(User.Name) -> User.Ids
    Rec = {'User.Ids', Values},
    'user_pb':encode_msg(Rec, 'User.Ids', []);
encode(<<1:10, 1:10, 3:12>>, Values) -> %% Search(User.RangeRequest) -> User.RangeResult
    Rec = {'User.RangeResult', [begin list_to_tuple(['User.Person' | V]) end||V <- Values]},
    'user_pb':encode_msg(Rec, 'User.RangeResult', []);
encode(<<1:10, 2:10, 1:12>>, Values) -> %% Admin(User.Name) -> User.Person
    Rec = list_to_tuple(['User.Person' | Values]),
    'user_pb':encode_msg(Rec, 'User.Person', []);
encode(Cmd, _Values) ->
    throw({error, cmd_not_found, Cmd}).
