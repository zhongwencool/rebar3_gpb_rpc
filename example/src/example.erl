-module(example).

%% -include_lib("gpb/include/gpb.hrl").
%% API
-export([decode/1]).

decode(Bin) ->
     rpc_router:decode(Bin).

