cp ../src/rebar3_gpb_rpc* _build/default/plugins/rebar3_gpb_rpc/src/
cp ../priv/* _build/default/plugins/rebar3_gpb_rpc/priv/
rm -rf _build/default/plugins/rebar3_gpb_rpc/ebin/
DEBUG=1 rebar3 gpb_rpc gen



