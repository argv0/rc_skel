%% -------------------------------------------------------------------
%%
%% rc_skel_vnode: VNode Implementation
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
-module(rc_skel_vnode).
-behaviour(riak_core_vnode).

%% API
-export([start_vnode/1]).

%% riak_core_vnode API
-export([init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {idx,
                in_handoff = false :: boolean()}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, rc_skel_vnode).


init([Index]) ->
    {ok, #state{idx=Index}}.


%% Commands originating from inside this vnode
handle_command(Cmd, Sender,State) ->
    io:format("command: ~p: ~p: ~p~n", [Cmd, Sender, State]),
    {reply, ok, State}.

handle_handoff_command(Cmd, Sender, State) ->
    handle_command(Cmd, Sender, State).

handoff_starting(_TargetNode, State) ->
    {true, State#state{in_handoff=true}}.

handoff_cancelled(State) ->
    {ok, State#state{in_handoff=false}}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_BinObj, State) ->
    {reply, ok, State}.

encode_handoff_item({_B,_K}, _V) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

terminate(_Reason, #state{}) ->
    ok.

