%% -------------------------------------------------------------------
%%
%% rc_skel_js_sup: supervise JavaScript VMs
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc supervise JavaScript VMs

-module(rc_skel_js_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, stop/1]).
-export([start_js/2]).

start_js(Manager, Pool) when is_pid(Manager) ->
    supervisor:start_child(?MODULE, [Manager, Pool]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_S) -> ok.

%% @private
init([]) ->
    {ok,
     {{simple_one_for_one, 10, 10},
      [{undefined,
        {rc_skel_js_vm, start_link, []},
        temporary, 2000, worker, [riak_kv_js_vm]}]}}.
