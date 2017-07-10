---
title: Erlang源码学习--gen_fsm
date: 2017-07-10 10:55:57
tags: Erlang
category: Erlang
---

### 简介
&emsp;&emsp;gen_fsm是通用的有限状态机模型，通过event触发不同状态下的事件，然后切换到不同状态和数据。

<!--- more --->

### 回调函数
init/1是调用start/3,4或者start_link/3,4启动gen_fsm进程时的回调函数，进行进程初始化工作；handle_event/3是send_all_state_event/2的回调函数，通用的事件处理函数；handle_sync_event/3是sync_send_all_state_event/2,3的回调函数，等同于handle_event/3，不过是同步消息，调用进程等待结果返回；handle_info/3是除了异步和同步消息外的其他消息回调函数；terminate/3是gen_fsm进程结束时的回调函数。StateName/2,3是状态变化的回调函数，当收到{'$gen_event', Event}或者{'$gen_event', From, Event}时的回调函数，是gen_fsm每个状态的函数，有多少个状态就需要实现多少个函数。
``` Erlang
-callback init(Args :: term()) ->
    {ok, StateName :: atom(), StateData :: term()} |
    {ok, StateName :: atom(), StateData :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_event(Event :: term(), StateName :: atom(),
                       StateData :: term()) ->
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
     timeout() | hibernate} |
    {stop, Reason :: term(), NewStateData :: term()}.
-callback handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
                            StateName :: atom(), StateData :: term()) ->
    {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
    {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
     timeout() | hibernate} |
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
     timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
    {stop, Reason :: term(), NewStateData :: term()}.
-callback handle_info(Info :: term(), StateName :: atom(),
                      StateData :: term()) ->
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
     timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewStateData :: term()}.
-callback terminate(Reason :: normal | shutdown | {shutdown, term()}
            | term(), StateName :: atom(), StateData :: term()) ->
    term().
-callback code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
              StateData :: term(), Extra :: term()) ->
    {ok, NextStateName :: atom(), NewStateData :: term()}.
```

### start/3,4;start_link/3,4
调用gen/5,6同步启动gen_fsm进程，区别在于start/3,4启动进程不主动和gen_fsm link，start_link则会。gen回调回init_it/6函数，这里回调到gen_fsm的回调函数Mod:init/1进行初始化工作，返回当前StateName和StateData，然后给启动进程返回gen_fsm进程Pid，然后gen_fsm进程loop/7循环，等待消息。gen_fsm没有主动trap_exit，所以最好在init/1回调函数里进行设置。start/3,4启动的gen_fsm的父进程为自己，start_link/3,4的为启动进程。
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
    Debug = gen:debug_options(Options),
    case catch Mod:init(Args) of
    {ok, StateName, StateData} ->
        proc_lib:init_ack(Starter, {ok, self()}),
        loop(Parent, Name, StateName, StateData, Mod, infinity, Debug);
    {ok, StateName, StateData, Timeout} ->
        proc_lib:init_ack(Starter, {ok, self()}),
        loop(Parent, Name, StateName, StateData, Mod, Timeout, Debug);
    {stop, Reason} ->
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

### send_event/2;sync_send_event/2,3
异步和同步给gen_fsm进程发事件，触发状态变化。gen_fsm进程收到{'$gen_event', Event}异步消息后，回调到当前状态的函数Mod:StateName/2函数；收到{'$gen_event', From, Event}同步消息后，回调到Mod:StateName/3函数，调用进程则同步等待结果返回。回调函数根据不同的event和StateData，返回不同的状态StateName和数据StateData。如果Mod:StateName/3返回{next_state, ...}时，需要在回调函数内显式调用gen_fsm:reply/2给调用sync_send_event/2,3进程回结果，否则sync_send_event/2,3会收到超时消息或者无限等待。
``` Erlang
send_event({global, Name}, Event) ->
    catch global:send(Name, {'$gen_event', Event}),
    ok;
send_event({via, Mod, Name}, Event) ->
    catch Mod:send(Name, {'$gen_event', Event}),
    ok;
send_event(Name, Event) ->
    Name ! {'$gen_event', Event},
    ok.
sync_send_event(Name, Event) ->
    case catch gen:call(Name, '$gen_sync_event', Event) of
    {ok,Res} ->
        Res;
    {'EXIT',Reason} ->
        exit({Reason, {?MODULE, sync_send_event, [Name, Event]}})
    end.

sync_send_event(Name, Event, Timeout) ->
    case catch gen:call(Name, '$gen_sync_event', Event, Timeout) of
    {ok,Res} ->
        Res;
    {'EXIT',Reason} ->
        exit({Reason, {?MODULE, sync_send_event, [Name, Event, Timeout]}})
    end.

dispatch({'$gen_event', Event}, Mod, StateName, StateData) ->
    Mod:StateName(Event, StateData);
dispatch({'$gen_sync_event', From, Event}, Mod, StateName, StateData) ->
    Mod:StateName(Event, From, StateData);
```

### send_all_state_event/2;sync_send_all_state_event/2,3
异步和同步给en_fsm进程发通用的事件，触发状态变化。gen_fsm进程收到{'$gen_all_state_event'， Event}异步消息后，回调到通用的Mod:handle_event/3函数来处理event；收到{'$gen_sync_all_state_event', From, Event}同步消息后，回调到Mod:handel_sync_event/3函数，调用进程等待消息返回。相比send_event/2是回调当前状态的函数，通用事件是由handle_event/3统一处理。如果handle_sync_event/3返回{next_state, ...}时，需要在回调函数内显式调用gen_fsm:reply/2给调用sync_send_all_state_event/2,3进程回结果，否则sync_send_all_state_event/2,3会收到超时消息或者无限等待。
``` Erlang
send_all_state_event({global, Name}, Event) ->
    catch global:send(Name, {'$gen_all_state_event', Event}),
    ok;
send_all_state_event({via, Mod, Name}, Event) ->
    catch Mod:send(Name, {'$gen_all_state_event', Event}),
    ok;
send_all_state_event(Name, Event) ->
    Name ! {'$gen_all_state_event', Event},
    ok.

sync_send_all_state_event(Name, Event) ->
    case catch gen:call(Name, '$gen_sync_all_state_event', Event) of
    {ok,Res} ->
        Res;
    {'EXIT',Reason} ->
        exit({Reason, {?MODULE, sync_send_all_state_event, [Name, Event]}})
    end.

sync_send_all_state_event(Name, Event, Timeout) ->
    case catch gen:call(Name, '$gen_sync_all_state_event', Event, Timeout) of
    {ok,Res} ->                                                                                                                                                      Res;
    {'EXIT',Reason} ->
        exit({Reason, {?MODULE, sync_send_all_state_event,
               [Name, Event, Timeout]}})
    end.

dispatch({'$gen_all_state_event', Event}, Mod, StateName, StateData) ->
    Mod:handle_event(Event, StateName, StateData);
dispatch({'$gen_sync_all_state_event', From, Event},
     Mod, StateName, StateData) ->
    Mod:handle_sync_event(Event, From, StateName, StateData);
```

### terminate
当gen_fsm进程处理消息返回{stop,...}或者处理异常，亦或父进程结束时，gen_fsm进程会关闭，回调到terminate/3函数。由于gen_fsm没有主动trap_exit，所以跟其link进程结束时，也会导致gen_fsm进程结束，而且不会回调terminate/3。
``` Erlang
terminate(Reason, Name, Msg, Mod, StateName, StateData, Debug) ->
    case catch Mod:terminate(Reason, StateName, StateData) of
    {'EXIT', R} ->
        error_info(R, Name, Msg, StateName, StateData, Debug),
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
                    FmtStateData =
                        case erlang:function_exported(Mod, format_status, 2) of
                            true ->
                                Args = [get(), StateData],
                                case catch Mod:format_status(terminate, Args) of
                                    {'EXIT', _} -> StateData;
                                    Else -> Else
                                end;
                            _ ->
                                StateData
                        end,
            error_info(Reason,Name,Msg,StateName,FmtStateData,Debug),
            exit(Reason)
        end
    end.
```

### 总结
&emsp;&emsp;gen_fsm是通用有限状态机模型进程，通过event触发不同的状态的数据。gen_fsm需要实现不同状态的回调函数，同时不主要trap_exit，在init/1回调函数时主动设置避免link进程结束时导致gen_fsm异常结束，并在handle_info/3加{'EXIT', ...}匹配来消费此类型消息。gen_fsm回调函数如handle_event/3，StateName/2,3非正常执行，返回异常时也会导致gen_fsm结束。
