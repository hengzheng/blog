---
title: Erlang源码学习--gen_server
date: 2017-06-19 14:41:39
tags: Erlang
category: Erlang
---

### 简介
&emsp;&emsp;gen_server是简单的client-server模型通用的服务器实现。通过call和cast来进行同步和异步消息请求。

<!-- more -->

### 回调函数
init/1是gen_server:start/3,4或者gen_server:start_link/3,4的回调函数，进行一些启动初始化的工作；handle_call/3是gen_server:call/2,3或者gen_server:multi_call/2,3,4的回调函数，同步请求消息；handle_cast/2是gen_server:cast/2或者gen_server:abcast/2,3的回调函数，处理异步消息；handle_info/2是除了call和cast外的其他消息处理；terminate/2是退出时的回调函数，进行退出的处理；code_change/3可用于更新gen_server的state。
``` Erlang
-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.
```

### start/3,4;start_link/3,4
启动gen_server进程，start和start_link的区别就是启动过程中，会不会主动跟启动的进程link在一起。这里调用gen:start/5,6来同步启动gen_server进程，Name定义gen_server进程的注册名（可以不设置），Mod参数为gen_server的回调模块，需要实现上面-callback定义的回调函数，Args是传递给Mod:init/1的参数，Options是选项参数，例如可以设置gen_server启动初始化超时时间({timeout, Time})。gen:start/5,6这里设置了回调模块为?MODULE，也就是gen_server，回调函数是init_it/6。这里可以看到执行完Mod:init/1后，返回gen_server的Pid，进程启动完毕，进入到loop/6循环中，等待消息。gen_server的父进程如果是start/3,4则为自己，而start_link/3,4则为启动进程。
``` Erlang
start(Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Mod, Args, Options).

start(Name, Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Name, Mod, Args, Options).

start_link(Mod, Args, Options) ->
    gen:start(?MODULE, link, Mod, Args, Options).

start_link(Name, Mod, Args, Options) ->
    gen:start(?MODULE, link, Name, Mod, Args, Options).

init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, Options) ->
    Name = name(Name0),
    Debug = debug_options(Name, Options),
    case catch Mod:init(Args) of
    {ok, State} ->
        proc_lib:init_ack(Starter, {ok, self()}),
        loop(Parent, Name, State, Mod, infinity, Debug);
    {ok, State, Timeout} ->
        proc_lib:init_ack(Starter, {ok, self()}),
        loop(Parent, Name, State, Mod, Timeout, Debug);
    {stop, Reason} ->
        %% For consistency, we must make sure that the
        %% registered name (if any) is unregistered before
        %% the parent process is notified about the failure.
        %% (Otherwise, the parent process could get
        %% an 'already_started' error if it immediately
        %% tried starting the process again.)
        unregister_name(Name0),
        proc_lib:init_ack(Starter, {error, Reason}),
        exit(Reason);
    ignore ->
        unregister_name(Name0),
        proc_lib:init_ack(Starter, ignore),
        exit(normal);
    {'EXIT', Reason} ->
        unregister_name(Name0),
        proc_lib:init_ack(Starter, {error, Reason}),
        exit(Reason);
    Else ->
        Error = {bad_return_value, Else},
        proc_lib:init_ack(Starter, {error, Error}),
        exit(Error)
    end.
```

### call/2,3
同步请求信息，可以设置超时时间，默认是5000ms。调用gen:call/3,4来进行同步请求，消息封装成{'$gen_call', From, Request}发到gen_server进程。gen_server进程调用回调函数Mod:handle_call/3处理call请求，并将结果返回给From进程。
``` Erlang
call(Name, Request) ->
    case catch gen:call(Name, '$gen_call', Request) of
    {ok,Res} ->
        Res;
    {'EXIT',Reason} ->
        exit({Reason, {?MODULE, call, [Name, Request]}})
    end.

call(Name, Request, Timeout) ->
    case catch gen:call(Name, '$gen_call', Request, Timeout) of
    {ok,Res} ->
        Res;
    {'EXIT',Reason} ->
        exit({Reason, {?MODULE, call, [Name, Request, Timeout]}})
    end.

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod) ->
    case catch Mod:handle_call(Msg, From, State) of
    {reply, Reply, NState} ->
        reply(From, Reply),
        loop(Parent, Name, NState, Mod, infinity, []);
    {reply, Reply, NState, Time1} ->
        reply(From, Reply),
        loop(Parent, Name, NState, Mod, Time1, []);
    {noreply, NState} ->
        loop(Parent, Name, NState, Mod, infinity, []);
    {noreply, NState, Time1} ->
        loop(Parent, Name, NState, Mod, Time1, []);
    {stop, Reason, Reply, NState} ->
        {'EXIT', R} =
        (catch terminate(Reason, Name, Msg, Mod, NState, [])),
        reply(From, Reply),
        exit(R);
    Other -> handle_common_reply(Other, Parent, Name, Msg, Mod, State)
    end.

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod, Debug) ->
    case catch Mod:handle_call(Msg, From, State) of
    {reply, Reply, NState} ->
        Debug1 = reply(Name, From, Reply, NState, Debug),
        loop(Parent, Name, NState, Mod, infinity, Debug1);
    {reply, Reply, NState, Time1} ->
        Debug1 = reply(Name, From, Reply, NState, Debug),
        loop(Parent, Name, NState, Mod, Time1, Debug1);
    {noreply, NState} ->
        Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                      {noreply, NState}),
        loop(Parent, Name, NState, Mod, infinity, Debug1);
    {noreply, NState, Time1} ->
        Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                      {noreply, NState}),
        loop(Parent, Name, NState, Mod, Time1, Debug1);
    {stop, Reason, Reply, NState} ->
        {'EXIT', R} =
        (catch terminate(Reason, Name, Msg, Mod, NState, Debug)),
        reply(Name, From, Reply, NState, Debug),
        exit(R);
    Other ->
        handle_common_reply(Other, Parent, Name, Msg, Mod, State, Debug)
    end.

```

### cast/2
异步给gen_server发消息。消息封装成{'$gen_cast', Request}发到gen_server进程。gen_server进程调用回调函数Mod:handle_cast/2处理cast消息。这里也可以看到除了call和cast消息外，其他消息回调到回调函数Mod:handle_info/2处理。

``` Erlang
cast({global,Name}, Request) ->
    catch global:send(Name, cast_msg(Request)),
    ok;
cast({via, Mod, Name}, Request) ->
    catch Mod:send(Name, cast_msg(Request)),
    ok;
cast({Name,Node}=Dest, Request) when is_atom(Name), is_atom(Node) ->
    do_cast(Dest, Request);
cast(Dest, Request) when is_atom(Dest) ->
    do_cast(Dest, Request);
cast(Dest, Request) when is_pid(Dest) ->
    do_cast(Dest, Request).

do_cast(Dest, Request) ->
    do_send(Dest, cast_msg(Request)),
    ok.

cast_msg(Request) -> {'$gen_cast',Request}.

handle_msg(Msg, Parent, Name, State, Mod) ->
    Reply = (catch dispatch(Msg, Mod, State)),
    handle_common_reply(Reply, Parent, Name, Msg, Mod, State).

dispatch({'$gen_cast', Msg}, Mod, State) ->
    Mod:handle_cast(Msg, State);
dispatch(Info, Mod, State) ->
    Mod:handle_info(Info, State).

handle_common_reply(Reply, Parent, Name, Msg, Mod, State, Debug) ->
    case Reply of
    {noreply, NState} ->
        Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                      {noreply, NState}),
        loop(Parent, Name, NState, Mod, infinity, Debug1);
    {noreply, NState, Time1} ->
        Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                      {noreply, NState}),
        loop(Parent, Name, NState, Mod, Time1, Debug1);
    {stop, Reason, NState} ->
        terminate(Reason, Name, Msg, Mod, NState, Debug);
    {'EXIT', What} ->
        terminate(What, Name, Msg, Mod, State, Debug);
    _ ->
        terminate({bad_return_value, Reply}, Name, Msg, Mod, State, Debug)
    end.
```

### terminate/6
gen_server如果收到父进程的exit消息，或者是handle_call,handle_cast,handle_info中指明stop行为或者发生了exit类型错误时，进程会退出。退出时回调到回调函数Mod:terminate/2，处理进程结束时的工作。
``` Erlang
decode_msg(Msg, Parent, Name, State, Mod, Time, Debug, Hib) ->
    case Msg of
    {system, From, get_state} ->
        sys:handle_system_msg(get_state, From, Parent, ?MODULE, Debug,
                  {State, [Name, State, Mod, Time]}, Hib);
    {system, From, {replace_state, StateFun}} ->
        NState = try StateFun(State) catch _:_ -> State end,
        sys:handle_system_msg(replace_state, From, Parent, ?MODULE, Debug,
                  {NState, [Name, NState, Mod, Time]}, Hib);
    {system, From, Req} ->
        sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
                  [Name, State, Mod, Time], Hib);
    {'EXIT', Parent, Reason} ->
        terminate(Reason, Name, Msg, Mod, State, Debug);
    _Msg when Debug =:= [] ->
        handle_msg(Msg, Parent, Name, State, Mod);
    _Msg ->
        Debug1 = sys:handle_debug(Debug, fun print_event/3,
                      Name, {in, Msg}),
        handle_msg(Msg, Parent, Name, State, Mod, Debug1)
    end.

terminate(Reason, Name, Msg, Mod, State, Debug) ->
    case catch Mod:terminate(Reason, State) of
    {'EXIT', R} ->
        error_info(R, Name, Msg, State, Debug),
        exit(R);
    _ ->
        case Reason of
        normal ->
            exit(normal);
        shutdown ->
            exit(shutdown);
        {shutdown,_}=Shutdown ->
            exit(Shutdown);
        _ ->
            FmtState =
            case erlang:function_exported(Mod, format_status, 2) of
                true ->
                Args = [get(), State],
                case catch Mod:format_status(terminate, Args) of
                    {'EXIT', _} -> State;
                    Else -> Else
                end;
                _ ->
                State
            end,
            error_info(Reason, Name, Msg, FmtState, Debug),
            exit(Reason)
        end
    end.
```

### 总结
&emsp;&emsp;gen_server进程不主动捕获exit信号，即没有设置process_flag(trap_exit, true)，所以在收到非normal的exit信号时，进程会退出。如果设置了trap_exit，则exit信号会转换成{'EXIT', Pid, Reason}消息进入到消息队列中。有个例外是，如果收到exit信号的reason是kill（即exit(Pid, kill)），则不管有没有设置trap_exit，进程都会退出。如果没有设置trap_exit或者是收到kill的exit消息进程退出时，gen_server不会调用terminate/6，而是直接退出，所以也不会回调Mod:terminate/2函数。
