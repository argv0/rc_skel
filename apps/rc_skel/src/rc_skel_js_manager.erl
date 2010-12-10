%% -------------------------------------------------------------------
%%
%% rc_skel_js_manager: dispatch work to JavaScript VMs
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

%% @doc dispatch work to JavaScript VMs
-module(rc_skel_js_manager).
-author('Kevin Smith <kevin@basho.com>').
-author('John Muellerleile <johnm@basho.com>').

-behaviour(gen_server).

%% API
-export([start_link/2,
         add_vm/1,
         reload/1,
         reload/2,
         mark_idle/1,
         reserve_vm/1,
         reserve_batch_vm/2,
         dispatch/3,
         blocking_dispatch/3,
         pool_size/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record('DOWN', {ref, type, pid, info}).
-record(vm_state, {pid, needs_reload=false}).
-record(state, {name, master, idle, reserve}).

start_link(Name, ChildCount) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, ChildCount], []).

reload(Name, []) ->
    reload(Name).
reload(Name) ->
    gen_server:call(Name, reload_vms).

add_vm(Name) ->
    gen_server:cast(Name, {add_vm, self()}).

mark_idle(Name) ->
    gen_server:call(Name, {mark_idle, self()}).

dispatch(Name, JSCall, Tries) ->
    dispatch(Name, JSCall, Tries, Tries).

blocking_dispatch(Name, JSCall, Tries) ->
    blocking_dispatch(Name, JSCall, Tries, Tries).

reserve_vm(Name) ->
    gen_server:call(Name, reserve_vm).

reserve_batch_vm(Name, Tries) ->
    reserve_batch_vm(Name, Tries, Tries).

pool_size(Name) ->
    gen_server:call(Name, pool_size).

init([Name, ChildCount]) ->
    Master = ets:new(Name, [private, {keypos, 2}]),
    Idle = ets:new(Name, [private]),
    start_vms(Name, ChildCount),
    {ok, #state{name=Name, master=Master, idle=Idle}}.

handle_call({mark_idle, VM}, _From, #state{master=Master,
                                           idle=Idle}=State) ->
    case needs_reload(Master, VM) of
        true ->
            rc_skel_js_vm:reload(VM),
            clear_reload(Master, VM);
        false ->
            ok
    end,
    ets:insert(Idle, {VM}),
    {reply, ok, State};

handle_call(reload_vms, _From, #state{master=Master, idle=Idle}=State) ->
    reload_idle_vms(Idle),
    mark_pending_reloads(Master, Idle),
    {reply, ok, State};

handle_call(reserve_batch_vm, _From, State) ->
    {Reply, State1} = case handle_call(reserve_vm, _From, State) of
                          {reply, {ok, VM}, NewState} ->
                              rc_skel_js_vm:start_batch(VM),
                              {{ok, VM}, NewState};
                          {reply, Error, NewState} ->
                              {Error, NewState}
                      end,
    {reply, Reply, State1};

handle_call(reserve_vm, _From, #state{idle=Idle}=State) ->
    Reply = case ets:first(Idle) of
                '$end_of_table' ->
                    {error, no_vms};
                VM ->
                    ets:delete(Idle, VM),
                    {ok, VM}
            end,
    {reply, Reply, State};

handle_call(pool_size, _From, #state{idle=Idle}=State) ->
    {reply, ets:info(Idle, size), State};

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast({add_vm, VMPid}, #state{master=Master, idle=Idle}=State) ->
    erlang:monitor(process, VMPid),
    VMState = #vm_state{pid=VMPid},
    ets:insert(Master, VMState),
    ets:insert(Idle, {VMPid}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#'DOWN'{pid=Pid}, #state{master=Master, idle=Idle}=State) ->
    ets:delete(Master, Pid),
    ets:delete(Idle, Pid),
    rc_skel_js_sup:start_js(self()),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
needs_reload(Master, VMPid) ->
    [VMState] = ets:lookup(Master, VMPid),
    VMState#vm_state.needs_reload.

clear_reload(Master, VMPid) ->
    [VMState] = ets:lookup(Master, VMPid),
    VMState1 = VMState#vm_state{needs_reload=false},
    ets:insert(Master, VMState1).

is_vm_idle(Idle, VMPid) ->
    case ets:lookup(Idle, {VMPid}) of
        [] ->
            false;
        _ ->
            true
    end.

start_vms(_Pool, 0) ->
    ok;
start_vms(Pool, Count) ->
    rc_skel_js_sup:start_js(self(), Pool),
    start_vms(Pool, Count - 1).

reload_idle_vms(Tid) ->
    reload_idle_vms(ets:first(Tid), Tid).

reload_idle_vms('$end_of_table', _Tid) ->
    ok;
reload_idle_vms(Current, Tid) ->
    rc_skel_js_vm:reload(Current),
    reload_idle_vms(ets:next(Tid, Current), Tid).

mark_pending_reloads(Master, Idle) ->
    mark_pending_reloads(ets:first(Master), Master, Idle).

mark_pending_reloads('$end_of_table', _Master, _Idle) ->
    ok;
mark_pending_reloads(VMPid, Master, Idle) ->
    case is_vm_idle(Idle, VMPid) of
        true ->
            ok;
        false ->
            [VMState] = ets:lookup(Master, VMPid),
            VMState1 = VMState#vm_state{needs_reload=true},
            ets:insert(Master, VMState1)
    end,
    mark_pending_reloads(ets:next(Master, VMPid), Master, Idle).

dispatch(_Name, _JSCall, _MaxCount, 0) ->
    error_logger:info_msg("JS call failed: All VMs are busy.~n"),
    {error, no_vms};
dispatch(Name, JSCall, MaxCount, Count) ->
    case reserve_vm(Name) of
        {ok, VM} ->
            JobId = {VM, make_ref()},
            rc_skel_js_vm:dispatch(VM, self(), JobId, JSCall),
            {ok, JobId};
        {error, no_vms} ->
            back_off(MaxCount, Count),
            dispatch(Name, JSCall, MaxCount, Count - 1)
    end.

blocking_dispatch(_Name, _JSCall, _MaxCount, 0) ->
    error_logger:info_msg("JS call failed: All VMs are busy.~n"),
    {error, no_vms};
blocking_dispatch(Name, JSCall, MaxCount, Count) ->
    case reserve_vm(Name) of
        {ok, VM} ->
            JobId = {VM, make_ref()},
            rc_skel_js_vm:blocking_dispatch(VM, JobId, JSCall);
        {error, no_vms} ->
            back_off(MaxCount, Count),
            blocking_dispatch(Name, JSCall, MaxCount, Count - 1)
    end.

reserve_batch_vm(_Name, _MaxCount, 0) ->
    {error, no_vms};
reserve_batch_vm(Name, MaxCount, Count) ->
    case gen_server:call(Name, reserve_batch_vm) of
        {error, no_vms} ->
            back_off(MaxCount, Count),
            reserve_batch_vm(Name, MaxCount, Count - 1);
        {ok, VM} ->
            {ok, VM}
    end.

back_off(MaxCount, Count) ->
    ScalingFactor = (1 + (MaxCount - Count)) *
        (0.1 + random:uniform(100) * 0.001),
    timer:sleep(erlang:round(500 * ScalingFactor)).
