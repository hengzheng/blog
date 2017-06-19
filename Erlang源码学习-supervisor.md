---
title: Erlang源码学习--supervisor
date: 2017-06-15 14:08:14
tags: Erlang
category: Erlang
---

### 简介
&emsp;&emsp;supervisor是监控树管理进程，监控其管理的子进程的行为，比如启动，关闭，重启策略。supervisor进程是个gen_server行为的进程，其子进程可以在回调函数init/1中指明，然后随supervisor一起启动，也可以在supervisor启动后调用start_child/2来启动。子进程的启动是一个接一个的同步启动，而结束时则按启动顺序的反序进行。

<!-- more -->

### init/1
supervisor行为的回调函数。如果一个模块申明了-behaviour(supervisor).行为(不是必需)或者是作为supervisor的回调模块，则需要实现并导出init/1回调函数。
``` Erlang
-callback init(Args :: term()) ->
    {ok, {{RestartStrategy :: strategy(),
           MaxR            :: non_neg_integer(),
           MaxT            :: non_neg_integer()},
           [ChildSpec :: child_spec()]}}
    | ignore.
```

### start_link/2;start_link/3
启动supervisor进程，对应调用的是gen_server:start_link/3;gen_server_start_link/4。gen_server回调模块为supervisor，回调函数为init/1，init的参数为start_link的参数, 定义supervisor进程注册名（可忽略），回调模块和回调参数。gen_server回调函数init/1调用supervisor定义的回调模块对应的回调函数（Mod:init/1），然后初始化state，SupFlags包括三个参数，为{Strategy（重启策略）, MaxIntensity（一定时间内子进程重启次数）, Period（一定时间，单位为秒）}，如果StartSpec指明有child子进程，则启动。
``` Erlang
start_link(Mod, Args) ->
    gen_server:start_link(supervisor, {self, Mod, Args}, []).

start_link(SupName, Mod, Args) ->
    gen_server:start_link(SupName, supervisor, {SupName, Mod, Args}, []).

init({SupName, Mod, Args}) ->
    process_flag(trap_exit, true),
    case Mod:init(Args) of
    {ok, {SupFlags, StartSpec}} ->
        case init_state(SupName, SupFlags, Mod, Args) of
        {ok, State} when ?is_simple(State) -> % simple_one_for_one
            init_dynamic(State, StartSpec); % 只记录#child{}数据，没有子进程启动
        {ok, State} ->
            init_children(State, StartSpec); % 记录#child{}数据，同时启动子进程
        Error ->
            {stop, {supervisor_data, Error}}
        end;
    ignore ->
        ignore;
    Error ->
        {stop, {bad_return, {Mod, init, Error}}}
    end.

% supervisor的state
-record(state, {
        name,            % SupName or {self(), Mod}
        strategy,        % 重启策略（one_for_one|one_for_all|rest_for_one|simple_one_for_one）
        children = [],   % 子进程#child{}列表
        dynamics,        % 动态子进程(sets或者dict存储)
        intensity,       % 一定时间内最大重启次数
        period,          % 一定时间
        restarts = [],   % 最近重启的时间列表
        module,          % 回调模块
        args             % 回调函数init的参数
    }).

% supervisor的child
-record(child, {
        pid,            % pid
        name,           % 名字
        mfargs,         % 启动函数{M,F,A}
        restart_type,   % 重启类型（permanent | transient | temporary）
        shutdown,       % brutal_kill|关闭超时时间
        child_type,     % 类型（worker | supervisor）
        modules = []    % 回调模块
    }).
```

### 重启策略
supervisor的重启策略：
one_for_one：某个子进程结束了，只有该子进程会被重启；
one_for_all：某个子进程结束了，所有的子进程都将被重启；
rest_for_one：某个子进程结束了，启动顺序中其后面的子进程也会被重启；
simple_one_for_one：所有子进程动态启动，且行为一致（用同样的代码）。此类型不能使用delete_child/2和restart_child/2函数，terminate_child/2可以指定子进程的pid来结束子进程。
supervisor子进程重启类型：
permanent：如果进程结束，必定会被重启；
transient：进程只在非正常结束时，会被重启（结束原因不是normal，shutdown，{shutdown, _Other}）；
temporary：如果进程结束，不会被重启（不受supervisor重启策略影响，即使是one_for_all或者是rest_for_one）.

``` Erlang
% permanent类型的子进程
do_restart(permanent, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child, State#state.name),
    restart(Child, State);
do_restart(_, normal, Child, State) ->
    NState = state_del_child(Child, State),
    {ok, NState};
do_restart(_, shutdown, Child, State) ->
    NState = state_del_child(Child, State),
    {ok, NState};
do_restart(_, {shutdown, _Term}, Child, State) ->
    NState = state_del_child(Child, State),
    {ok, NState};
% 非正常结束的transient类型的子进程
do_restart(transient, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child, State#state.name),
    restart(Child, State);
% temporary类型的子进程
do_restart(temporary, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child, State#state.name),
    NState = state_del_child(Child, State),
    {ok, NState}.

% supervisor重启策略的相关代码
restart(simple_one_for_one, Child, State) ->
    #child{pid = OldPid, mfargs = {M, F, A}} = Child,
    Dynamics = ?DICT:erase(OldPid, dynamics_db(Child#child.restart_type,
                           State#state.dynamics)),
    case do_start_child_i(M, F, A) of
    {ok, Pid} ->
        NState = State#state{dynamics = ?DICT:store(Pid, A, Dynamics)},
        {ok, NState};
    {ok, Pid, _Extra} ->
        NState = State#state{dynamics = ?DICT:store(Pid, A, Dynamics)},
        {ok, NState};
    {error, Error} ->
        NState = State#state{dynamics = ?DICT:store(restarting(OldPid), A,
                            Dynamics)},
        report_error(start_error, Error, Child, State#state.name),
        {try_again, NState}
    end;
restart(one_for_one, Child, State) ->
    OldPid = Child#child.pid,
    case do_start_child(State#state.name, Child) of
        {ok, Pid} ->
        NState = replace_child(Child#child{pid = Pid}, State),
        {ok, NState};
    {ok, Pid, _Extra} ->
        NState = replace_child(Child#child{pid = Pid}, State),
        {ok, NState};
    {error, Reason} ->
        NState = replace_child(Child#child{pid = restarting(OldPid)}, State),
        report_error(start_error, Reason, Child, State#state.name),
        {try_again, NState}
    end;
restart(rest_for_one, Child, State) ->
    {ChAfter, ChBefore} = split_child(Child#child.pid, State#state.children),
    ChAfter2 = terminate_children(ChAfter, State#state.name),
    case start_children(ChAfter2, State#state.name) of
    {ok, ChAfter3} ->
        {ok, State#state{children = ChAfter3 ++ ChBefore}};
    {error, ChAfter3, {failed_to_start_child, ChName, _Reason}}
        when ChName =:= Child#child.name ->
        NChild = Child#child{pid=restarting(Child#child.pid)},
        NState = State#state{children = ChAfter3 ++ ChBefore},
        {try_again, replace_child(NChild,NState)};
    {error, ChAfter3, {failed_to_start_child, ChName, _Reason}} ->
        NChild = lists:keyfind(ChName, #child.name, ChAfter3),
        NChild2 = NChild#child{pid=?restarting(undefined)},
        NState = State#state{children = ChAfter3 ++ ChBefore},
        {try_again, replace_child(NChild2,NState), NChild2}
    end;
restart(one_for_all, Child, State) ->
    Children1 = del_child(Child#child.pid, State#state.children),
    Children2 = terminate_children(Children1, State#state.name),
    case start_children(Children2, State#state.name) of
        {ok, NChs} ->
            {ok, State#state{children = NChs}};
    {error, NChs, {failed_to_start_child, ChName, _Reason}}
      when ChName =:= Child#child.name ->
        NChild = Child#child{pid=restarting(Child#child.pid)},
        NState = State#state{children = NChs},
        {try_again, replace_child(NChild,NState)};
    {error, NChs, {failed_to_start_child, ChName, _Reason}} ->
        NChild = lists:keyfind(ChName, #child.name, NChs),
        NChild2 = NChild#child{pid=?restarting(undefined)},
        NState = State#state{children = NChs},
        {try_again, replace_child(NChild2,NState), NChild2}
    end.

```

### terminate/2
supervisor是gen_server行为的进程，所以结束时回调到terminate/2。simple_one_for_one的supervisor子进程的存储跟其他类型的不一样，所以特别处理，但是过程类似，都是先moniter子进程，然后根据shutdown类型发对应信息，然后等待子进程退出消息。supervisor子进程为supervisor类型时，shutdown超时时间最好为infinity，这样能确保子监控树有足够的时候来结束；如果子进程的shutdown类型是brutal_kill，则通过exit(Child, kill)来结束子进程，如果是个超时时间T，则先通过exit(Child, shutdown)，如果在T时间内没有shutdown关闭子进程的消息，则无条件的exit(Child, Kill)关闭子进程。子进程是supervisor，gen_server或者gen_fsm行为的进程，则modules是单个元素的列表[Module]，这里的Module为对应的回调模块；如果是gen_event行为则要设置在dynamic。
otp16RB03-1版本的代码这里有个bug，当shutdown是个超时时间时，超时后发kill消息那里的size不应该减1，查看最新版本的代码已经修复了。
``` Erlang
terminate(_Reason, #state{children=[Child]} = State) when ?is_simple(State) ->
    terminate_dynamic_children(Child, dynamics_db(Child#child.restart_type,
                                                  State#state.dynamics),
                               State#state.name);
terminate(_Reason, State) ->
    terminate_children(State#state.children, State#state.name).

terminate_children([Child = #child{restart_type=temporary} | Children], SupName, Res) ->
    do_terminate(Child, SupName),
    terminate_children(Children, SupName, Res);
terminate_children([Child | Children], SupName, Res) ->
    NChild = do_terminate(Child, SupName),
    terminate_children(Children, SupName, [NChild | Res]);
terminate_children([], _SupName, Res) ->
    Res.

do_terminate(Child, SupName) when is_pid(Child#child.pid) ->
    case shutdown(Child#child.pid, Child#child.shutdown) of
        ok ->
            ok;
        {error, normal} when Child#child.restart_type =/= permanent ->
            ok;
        {error, OtherReason} ->
            report_error(shutdown_error, OtherReason, Child, SupName)
    end,
    Child#child{pid = undefined};
do_terminate(Child, _SupName) ->
    Child#child{pid = undefined}.

shutdown(Pid, brutal_kill) ->
    case monitor_child(Pid) of
    ok ->
        exit(Pid, kill),
        receive
        {'DOWN', _MRef, process, Pid, killed} ->
            ok;
        {'DOWN', _MRef, process, Pid, OtherReason} ->
            {error, OtherReason}
        end;
    {error, Reason} ->
        {error, Reason}
    end;
shutdown(Pid, Time) ->
    case monitor_child(Pid) of
    ok ->
        exit(Pid, shutdown), %% Try to shutdown gracefully
        receive
        {'DOWN', _MRef, process, Pid, shutdown} ->
            ok;
        {'DOWN', _MRef, process, Pid, OtherReason} ->
            {error, OtherReason}
        after Time ->
            exit(Pid, kill),  %% Force termination.
            receive
            {'DOWN', _MRef, process, Pid, OtherReason} ->
                {error, OtherReason}
            end
        end;
    {error, Reason} ->
        {error, Reason}
    end.

%% Help function to shutdown/2 switches from link to monitor approach
monitor_child(Pid) ->
    %% Do the monitor operation first so that if the child dies
    %% before the monitoring is done causing a 'DOWN'-message with
    %% reason noproc, we will get the real reason in the 'EXIT'-message
    %% unless a naughty child has already done unlink...
    erlang:monitor(process, Pid),
    unlink(Pid),

    receive
    %% If the child dies before the unlik we must empty
    %% the mail-box of the 'EXIT'-message and the 'DOWN'-message.
    {'EXIT', Pid, Reason} ->
        receive
        {'DOWN', _, process, Pid, _} ->
            {error, Reason}
        end
    after 0 ->
        %% If a naughty child did unlink and the child dies before
        %% monitor the result will be that shutdown/2 receives a
        %% 'DOWN'-message with reason noproc.
        %% If the child should die after the unlink there
        %% will be a 'DOWN'-message with a correct reason
        %% that will be handled in shutdown/2.
        ok
    end.

terminate_dynamic_children(Child, Dynamics, SupName) ->
    {Pids, EStack0} = monitor_dynamic_children(Child, Dynamics),
    Sz = ?SETS:size(Pids),
    EStack = case Child#child.shutdown of
                 brutal_kill ->
                     ?SETS:fold(fun(P, _) -> exit(P, kill) end, ok, Pids),
                     wait_dynamic_children(Child, Pids, Sz, undefined, EStack0);
                 infinity ->
                     ?SETS:fold(fun(P, _) -> exit(P, shutdown) end, ok, Pids),
                     wait_dynamic_children(Child, Pids, Sz, undefined, EStack0);
                 Time ->
                     ?SETS:fold(fun(P, _) -> exit(P, shutdown) end, ok, Pids),
                     TRef = erlang:start_timer(Time, self(), kill),
                     wait_dynamic_children(Child, Pids, Sz, TRef, EStack0)
             end,
    %% Unroll stacked errors and report them
    ?DICT:fold(fun(Reason, Ls, _) ->
                       report_error(shutdown_error, Reason,
                                    Child#child{pid=Ls}, SupName)
               end, ok, EStack).
monitor_dynamic_children(#child{restart_type=temporary}, Dynamics) ->
    ?SETS:fold(fun(P, {Pids, EStack}) ->
                       case monitor_child(P) of
                           ok ->
                               {?SETS:add_element(P, Pids), EStack};
                           {error, normal} ->
                               {Pids, EStack};
                           {error, Reason} ->
                               {Pids, ?DICT:append(Reason, P, EStack)}
                       end
               end, {?SETS:new(), ?DICT:new()}, Dynamics);
monitor_dynamic_children(#child{restart_type=RType}, Dynamics) ->
    ?DICT:fold(fun(P, _, {Pids, EStack}) when is_pid(P) ->
                       case monitor_child(P) of
                           ok ->
                               {?SETS:add_element(P, Pids), EStack};
                           {error, normal} when RType =/= permanent ->
                               {Pids, EStack};
                           {error, Reason} ->
                               {Pids, ?DICT:append(Reason, P, EStack)}
                       end;
          (?restarting(_), _, {Pids, EStack}) ->
               {Pids, EStack}
               end, {?SETS:new(), ?DICT:new()}, Dynamics).
wait_dynamic_children(_Child, _Pids, 0, undefined, EStack) ->
    EStack;
wait_dynamic_children(_Child, _Pids, 0, TRef, EStack) ->
    %% If the timer has expired before its cancellation, we must empty the
    %% mail-box of the 'timeout'-message.
    erlang:cancel_timer(TRef),
    receive
        {timeout, TRef, kill} ->
            EStack
    after 0 ->
            EStack
    end;
wait_dynamic_children(#child{shutdown=brutal_kill} = Child, Pids, Sz,
                      TRef, EStack) ->
    receive
        {'DOWN', _MRef, process, Pid, killed} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                                  TRef, EStack);

        {'DOWN', _MRef, process, Pid, Reason} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                                  TRef, ?DICT:append(Reason, Pid, EStack))
    end;
wait_dynamic_children(#child{restart_type=RType} = Child, Pids, Sz,
                      TRef, EStack) ->
    receive
        {'DOWN', _MRef, process, Pid, shutdown} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                                  TRef, EStack);

        {'DOWN', _MRef, process, Pid, normal} when RType =/= permanent ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                                  TRef, EStack);

        {'DOWN', _MRef, process, Pid, Reason} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                                  TRef, ?DICT:append(Reason, Pid, EStack));

        {timeout, TRef, kill} ->
            ?SETS:fold(fun(P, _) -> exit(P, kill) end, ok, Pids),
            wait_dynamic_children(Child, Pids, Sz-1, undefined, EStack) % bug，这里的Sz不应该减一，查看最新版本19.3已经修改过。
    end.
```

### start_child/2;restart_child/2
start_child/2启动子进程时，如果是simple_one_for_one的supervisor，则参数为子进程的启动参数，其他则为子进程描述。子进程启动先判断是否存在，然后启动。restart_child/2不能用来重启supervisor为simple_one_for_one的子进程，首先判断是否子进程是否存在，且为未运行或者未被重启中，然后启动子进程。restart_child/2一般用于通过terminate_child/2结束的非temporary类型的子进程（此类型的在结束时子进程信息会删除）。

``` Erlang
start_child(Supervisor, ChildSpec) ->
    call(Supervisor, {start_child, ChildSpec}).
restart_child(Supervisor, Name) ->
    call(Supervisor, {restart_child, Name}).

handle_call({start_child, EArgs}, _From, State) when ?is_simple(State) ->
    Child = hd(State#state.children),
    #child{mfargs = {M, F, A}} = Child,
    Args = A ++ EArgs,
    case do_start_child_i(M, F, Args) of
    {ok, undefined} when Child#child.restart_type =:= temporary ->
        {reply, {ok, undefined}, State};
    {ok, Pid} ->
        NState = save_dynamic_child(Child#child.restart_type, Pid, Args, State),
        {reply, {ok, Pid}, NState};
    {ok, Pid, Extra} ->
        NState = save_dynamic_child(Child#child.restart_type, Pid, Args, State),
        {reply, {ok, Pid, Extra}, NState};
    What ->
        {reply, What, State}
    end;
do_start_child_i(M, F, A) ->
    case catch apply(M, F, A) of
    {ok, Pid} when is_pid(Pid) ->
        {ok, Pid};
    {ok, Pid, Extra} when is_pid(Pid) ->
        {ok, Pid, Extra};
    ignore ->
        {ok, undefined};
    {error, Error} ->
        {error, Error};
    What ->
        {error, What}
    end.
handle_call({start_child, ChildSpec}, _From, State) ->
    case check_childspec(ChildSpec) of
    {ok, Child} ->
        {Resp, NState} = handle_start_child(Child, State),
        {reply, Resp, NState};
    What ->
        {reply, {error, What}, State}
    end;
handle_call({restart_child, Name}, _From, State) ->
    case get_child(Name, State) of
    {value, Child} when Child#child.pid =:= undefined ->
        case do_start_child(State#state.name, Child) of
        {ok, Pid} ->
            NState = replace_child(Child#child{pid = Pid}, State),
            {reply, {ok, Pid}, NState};
        {ok, Pid, Extra} ->
            NState = replace_child(Child#child{pid = Pid}, State),
            {reply, {ok, Pid, Extra}, NState};
        Error ->
            {reply, Error, State}
        end;
    {value, #child{pid=?restarting(_)}} ->
        {reply, {error, restarting}, State};
    {value, _} ->
        {reply, {error, running}, State};
    _ ->
        {reply, {error, not_found}, State}
    end;
handle_start_child(Child, State) ->
    case get_child(Child#child.name, State) of
    false ->
        case do_start_child(State#state.name, Child) of
        {ok, undefined} when Child#child.restart_type =:= temporary ->
            {{ok, undefined}, State};
        {ok, Pid} ->
            {{ok, Pid}, save_child(Child#child{pid = Pid}, State)};
        {ok, Pid, Extra} ->
            {{ok, Pid, Extra}, save_child(Child#child{pid = Pid}, State)};
        {error, What} ->
            {{error, {What, Child}}, State}
        end;
    {value, OldChild} when is_pid(OldChild#child.pid) ->
        {{error, {already_started, OldChild#child.pid}}, State};
    {value, _OldChild} ->
        {{error, already_present}, State}
    end.
do_start_child(SupName, Child) ->
    #child{mfargs = {M, F, Args}} = Child,
    case catch apply(M, F, Args) of
    {ok, Pid} when is_pid(Pid) ->
        NChild = Child#child{pid = Pid},
        report_progress(NChild, SupName),
        {ok, Pid};
    {ok, Pid, Extra} when is_pid(Pid) ->
        NChild = Child#child{pid = Pid},
        report_progress(NChild, SupName),
        {ok, Pid, Extra};
    ignore ->
        {ok, undefined};
    {error, What} -> {error, What};
    What -> {error, What}
    end.
```

### 总结
&emsp;&emsp;supervisor是gen_server行为的进程，通常作为application的监控进程。supervisor根据不同的重启策略，结合子进程的重启类型来管理子进程的重启。为了避免子进程无限重启，supervisor有重启频率限制｛MaxR，MaxT}(一定时间内重启次数)，如果超过了这个设置，supervisor会结束所有子进程，同时自己也退出。
