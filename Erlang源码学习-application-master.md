---
title: Erlang源码学习--application_master
date: 2017-06-12 11:04:31
tags: Erlang
category: Erlang
---

### 简介
&emsp;&emsp;application_master是OTP application的管理进程。application_controller控制Erlang虚拟机中所有的application，而application_master则管理单个application。application配置数据中的mod字段定义了application_master的回调模块和启动参数，回调函数start/5启动的通常为supervisor进程。

<!-- more -->

### start_link/2
start_link/2通过proc_lib:start_link/3创建同步进程调用init/4来启动application_master进程。init/4首先和application_controller(Parent)建立link，设置group_leader为自己（相同group的进程IO都会被导到group_leader）；接着ac_tab中插入master进程信息，设置state；然后调用start_it/2来回调mod字段字义的模块启动函数。start_it/2启动新进程（因为application_master是group_leader，还要处理IO）来执行启动逻辑start_it/4，此时init/4进入到init_loop/4循环。start_it/4调用M:start/2启动监控树，然后返回自己Pid给init/4，进入到loop_it/4循环。init/4收到start_it返回的Pid后，记录到#state.child中，然后给上级的starter返回init/4进程Pid，此时application_master启动完毕。

``` Erlang
start_link(ApplData, Type) ->
    Parent = whereis(application_controller),
    proc_lib:start_link(application_master, init,
            [Parent, self(), ApplData, Type]).

init(Parent, Starter, ApplData, Type) ->
    link(Parent),
    process_flag(trap_exit, true),
    OldGleader = group_leader(),
    group_leader(self(), self()),
    %% Insert ourselves as master for the process.  This ensures that
    %% the processes in the application can use get_env/1 at startup.
    Name = ApplData#appl_data.name,
    ets:insert(ac_tab, {{application_master, Name}, self()}),
    State = #state{appl_data = ApplData, gleader = OldGleader},
    case start_it(State, Type) of
        {ok, Pid} ->          % apply(M,F,A) returned ok
            set_timer(ApplData#appl_data.maxT),
            unlink(Starter),
            proc_lib:init_ack(Starter, {ok,self()}),
            main_loop(Parent, State#state{child = Pid});
        {error, Reason} ->    % apply(M,F,A) returned error
            exit(Reason);
        Else ->               % apply(M,F,A) returned erroneous
            exit(Else)
    end.

start_it(State, Type) ->
    Tag = make_ref(),
    Pid = spawn_link(application_master, start_it, [Tag, State, self(), Type]),
    init_loop(Pid, Tag, State, Type).

start_it_old(Tag, From, Type, ApplData) ->
    {M,A} = ApplData#appl_data.mod,
    case catch M:start(Type, A) of
    {ok, Pid} ->
        link(Pid),
        From ! {Tag, {ok, self()}},
        loop_it(From, Pid, M, []);
    {ok, Pid, AppState} ->
        link(Pid),
        From ! {Tag, {ok, self()}},
        loop_it(From, Pid, M, AppState);
    {'EXIT', normal} ->
        From ! {Tag, {error, {{'EXIT',normal},{M,start,[Type,A]}}}};
    {error, Reason} ->
        From ! {Tag, {error, {Reason, {M,start,[Type,A]}}}};
    Other ->
        From ! {Tag, {error, {bad_return,{{M,start,[Type,A]},Other}}}}
    end.
```
![启动时序图](application_master.png)

### stop/1
通过给application_master发stop消息来结束进程，这里通过receive来进行同步关闭。application_master收到stop消息后，调用terminate/2来结束#state.child和#state.children里记录的进程。terminate_child/2通过给Child发terminate来结束进程，Child进程也就是start_it/2里启动的进程，然后applicatin_master进入到terminate_loop/2循环，等待Child进程结束。kill_children/2通过给进程发{'EXIT', Parent, kill}消息（exit(Pid, kill)）来结束，同时用同样的方式结束没有记录在#state.children而group_leader是application_master的进程。Child进程在收到terminate消息后，先执行回调模块的prep_stop/2函数，进行结束前的一些处理，然后给M:start/2启动的监控树进程发送{'EXIT', Parent, shutdown}（exit(Pid, shutdown)）消息来结束监控树，最后再调用回调模块的stop/1函数，此时Child关闭。
``` Erlang
stop(AppMaster) -> call(AppMaster, stop).

call(AppMaster, Req) ->
    Tag = make_ref(),
    Ref = erlang:monitor(process, AppMaster),
    AppMaster ! {Req, Tag, self()},
    receive
    {'DOWN', Ref, process, _, _Info} ->
        ok;
    {Tag, Res} ->
        erlang:demonitor(Ref, [flush]),
        Res
    end.

terminate(Reason, State) ->
    terminate_child(State#state.child, State),
    kill_children(State#state.children),
    exit(Reason).

terminate_child_i(Child, State) ->
    Child ! {self(), terminate},
    terminate_loop(Child, State).

%% Try to shutdown the child gently
terminate_child(undefined, _) -> ok;
terminate_child(Child, State) ->
    terminate_child_i(Child, State).

kill_children(Children) ->
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Children),
    kill_all_procs().

kill_all_procs() ->
    kill_all_procs_1(processes(), self(), 0).

kill_all_procs_1([Self|Ps], Self, N) ->
    kill_all_procs_1(Ps, Self, N);
kill_all_procs_1([P|Ps], Self, N) ->
    case process_info(P, group_leader) of
    {group_leader,Self} ->
        exit(P, kill),
        kill_all_procs_1(Ps, Self, N+1);
    _ ->
        kill_all_procs_1(Ps, Self, N)
    end;
kill_all_procs_1([], _, 0) -> ok;
kill_all_procs_1([], _, _) -> kill_all_procs().
```

### 总结
&emsp;&emsp;application_master进程启动时是同步的，而同时又作为group_leader进程，假如M:start/2函数里有IO请求，就会造成死锁，所以需要额外新起个start_it/4进程来执行启动过程。application_master进程是application这组集合的group_leader。
