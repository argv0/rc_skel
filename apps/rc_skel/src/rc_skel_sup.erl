-module(rc_skel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    VMaster = {rc_skel_vnode_master,
               {riak_core_vnode_master, start_link, [rc_skel_vnode]},
               permanent, 5000, worker, [riak_core_vnode_master]},
    JSPool = {rc_skel_js_pool,
              {rc_skel_js_manager, start_link, [rc_skel_js_pool, 10]},
               permanent, 30000, worker, [rc_skel_js_manager]},
    JSSup = {rc_skel_js_sup,
             {rc_skel_js_sup, start_link, []},
             permanent, infinity, supervisor, [rc_skel_js_sup]},
    {ok, { {one_for_one, 5, 10}, [VMaster, JSSup, JSPool]} }.

