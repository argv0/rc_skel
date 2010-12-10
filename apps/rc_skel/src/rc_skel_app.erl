-module(rc_skel_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Spin up supervisor
    case rc_skel_sup:start_link() of
        {ok, Pid} ->
            %% Go ahead and mark the riak_kv service as up in the node watcher.
            %% The riak_core_ring_handler blocks until all vnodes have been started
            %% synchronously.
            riak_core:register_vnode_module(rc_skel_vnode),
            riak_core_node_watcher:service_up(rc_skel, self()),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
