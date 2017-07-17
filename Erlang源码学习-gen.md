---
title: Erlang源码学习--gen
date: 2017-07-17 14:24:53
tags: Erlang
category: Erlang
---
### 简介
&emsp;&emsp;gen模块为标准行为（gen_server,gen_event,gen_fsm）的启动和同步请求提供通用的接口函数。使用gen启动行为的模块需要实现init_it/6的回调函数。

<!-- more -->

### start/5,6
通过调用prob_lib:start/5,prob_lib:start_link/5来同步启动进程。start/5,6的区别在于是否给新启动的进程注册名。其中参数GenMod是gen的回调模块，需要实现init_it/6函数；LinkP是prob_lib:start还是start_link的标识，也即是启动的新进程是否主动和启动它的进程link；Name为新进程注册名方式；Mod是通过gen启动进程的回调模块；Args是Mod回调模块init/1的参数；Options是配置选项列表，比如timeout配置等。prob_lib:start/5,prob_lib:start_link/5参数分别是M,F,A,Timeout, Opts，因为启动新进程执行gen:init_it/7函数，也即是回调到GenMod:init_it/6函数。
``` Erlang
start(GenMod, LinkP, Name, Mod, Args, Options) ->
    case where(Name) of
    undefined ->
        do_spawn(GenMod, LinkP, Name, Mod, Args, Options);
    Pid ->
        {error, {already_started, Pid}}
    end.
start(GenMod, LinkP, Mod, Args, Options) ->
    do_spawn(GenMod, LinkP, Mod, Args, Options).

do_spawn(GenMod, link, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start_link(?MODULE, init_it,
            [GenMod, self(), self(), Mod, Args, Options],
            Time,
            spawn_opts(Options));
do_spawn(GenMod, _, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start(?MODULE, init_it,
           [GenMod, self(), self, Mod, Args, Options],
           Time,
           spawn_opts(Options)).

do_spawn(GenMod, link, Name, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start_link(?MODULE, init_it,
            [GenMod, self(), self(), Name, Mod, Args, Options],
            Time,
            spawn_opts(Options));
do_spawn(GenMod, _, Name, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start(?MODULE, init_it,
           [GenMod, self(), self, Name, Mod, Args, Options],
           Time,
           spawn_opts(Options));

init_it(GenMod, Starter, Parent, Mod, Args, Options) ->
    init_it2(GenMod, Starter, Parent, self(), Mod, Args, Options).

init_it(GenMod, Starter, Parent, Name, Mod, Args, Options) ->
    case name_register(Name) of
    true ->
        init_it2(GenMod, Starter, Parent, Name, Mod, Args, Options);
    {false, Pid} ->
        proc_lib:init_ack(Starter, {error, {already_started, Pid}})
    end.

init_it2(GenMod, Starter, Parent, Name, Mod, Args, Options) ->
    GenMod:init_it(Starter, Parent, Name, Mod, Args, Options).
```

### call/3,4
向进程发送同步请求，默认超时时间为5000ms。首先尝试监控需要同步请求消息的进程，因为在同步请求结果返回前，有可能进程就已经退出了，然后同步等级请求结果返回。如果直接监控进程失败了，则尝试监控节点。
``` Erlang
call(Process, Label, Request) ->
    call(Process, Label, Request, ?default_timeout).
call(Pid, Label, Request, Timeout)
  when is_pid(Pid), Timeout =:= infinity;
       is_pid(Pid), is_integer(Timeout), Timeout >= 0 ->
    do_call(Pid, Label, Request, Timeout);
call(Name, Label, Request, Timeout)
  when is_atom(Name), Timeout =:= infinity;
       is_atom(Name), is_integer(Timeout), Timeout >= 0 ->
    case whereis(Name) of
    Pid when is_pid(Pid) ->
        do_call(Pid, Label, Request, Timeout);
    undefined ->
        exit(noproc)
    end;
call(Process, Label, Request, Timeout)
    when ((tuple_size(Process) == 2 andalso element(1, Process) == global)
    orelse
    (tuple_size(Process) == 3 andalso element(1, Process) == via))
       andalso
       (Timeout =:= infinity orelse (is_integer(Timeout) andalso Timeout >= 0)) ->
    case where(Process) of
    Pid when is_pid(Pid) ->
        Node = node(Pid),
        try do_call(Pid, Label, Request, Timeout)
        catch
        exit:{nodedown, Node} ->
            %% A nodedown not yet detected by global,
            %% pretend that it was.
            exit(noproc)
        end;
    undefined ->
        exit(noproc)
    end;
call({Name, Node}, Label, Request, Timeout)
  when Node =:= node(), Timeout =:= infinity;
        Node =:= node(), is_integer(Timeout), Timeout >= 0 ->
    call(Name, Label, Request, Timeout);
call({_Name, Node}=Process, Label, Request, Timeout)
  when is_atom(Node), Timeout =:= infinity;
       is_atom(Node), is_integer(Timeout), Timeout >= 0 ->
    if
    node() =:= nonode@nohost ->
        exit({nodedown, Node});
    true ->
        do_call(Process, Label, Request, Timeout)
    end.
do_call(Process, Label, Request, Timeout) ->
    try erlang:monitor(process, Process) of
    Mref ->
        %% If the monitor/2 call failed to set up a connection to a
        %% remote node, we don't want the '!' operator to attempt
        %% to set up the connection again. (If the monitor/2 call
        %% failed due to an expired timeout, '!' too would probably
        %% have to wait for the timeout to expire.) Therefore,
        %% use erlang:send/3 with the 'noconnect' option so that it
        %% will fail immediately if there is no connection to the
        %% remote node.

        catch erlang:send(Process, {Label, {self(), Mref}, Request},
          [noconnect]),
        receive
        {Mref, Reply} ->
            erlang:demonitor(Mref, [flush]),
            {ok, Reply};
        {'DOWN', Mref, _, _, noconnection} ->
            Node = get_node(Process),
            exit({nodedown, Node});
        {'DOWN', Mref, _, _, Reason} ->
            exit(Reason)
        after Timeout ->
            erlang:demonitor(Mref, [flush]),
            exit(timeout)
        end
    catch
    error:_ ->
        %% Node (C/Java?) is not supporting the monitor.
        %% The other possible case -- this node is not distributed
        %% -- should have been handled earlier.
        %% Do the best possible with monitor_node/2.
        %% This code may hang indefinitely if the Process
        %% does not exist. It is only used for featureweak remote nodes.
        Node = get_node(Process),
        monitor_node(Node, true),
        receive
        {nodedown, Node} ->
            monitor_node(Node, false),
            exit({nodedown, Node})
        after 0 ->
            Tag = make_ref(),
            Process ! {Label, {self(), Tag}, Request},
            wait_resp(Node, Tag, Timeout)
        end
    end.

```

### 总结
&emsp;&emsp;gen是通用的接口模块，为gen_server，gen_fsm，gen_event标准行为提供同步启动和同步请求消息提供API。
