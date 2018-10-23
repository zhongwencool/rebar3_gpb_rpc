-module(example).

%% -include_lib("gpb/include/gpb.hrl").
%% API
-export([decode/1, encode/3]).

% Package:10, Service:10, RpcFunc:12
decode(Bin) ->
    {Mod, IRec, ORec} = router:decode(Bin).

encode(RouterCmd, ORec, MapList) ->
    
    ok.

