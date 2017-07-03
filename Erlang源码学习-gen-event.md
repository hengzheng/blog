---
title: Erlang源码学习--gen_event
date: 2017-07-03 11:54:02
tags: Erlang
category: Erlang
---

### 简介
&emsp;&emsp;gen_event进程是event handler的管理进程。一个handler对应一个state状态，可动态添加和删除。对于gen_event的每个event，各个handler单独处理，互不影响，互不相关。

<!-- more -->

### 回调函数
gen_event行为的回调函数，也即是gen_event handler模块需要实现的函数。init/1是handler初始化函数，返回state，添加handler或者切换（swap）handler到gen_event时被调用。handle_event/2是handler处理event的回调函数，当gen_event收到notify消息时，各handler调用此函数处理event。handle_call/2是handler处理call的回调函数，当gen_event收到call消息时，回调到指定的handler。handle_info/2是handler处理除notify和call外的其他消息。terminate/2是删除handler时的回调函数，处理handler结束时的工作。code_change/3是系统更新指定handler state的回调函数。

``` Erlang
-callback init(InitArgs :: term()) ->
    {ok, State :: term()} |
    {ok, State :: term(), hibernate} |
    {error, Reason :: term()}.
-callback handle_event(Event :: term(), State :: term()) ->
    {ok, NewState :: term()} |
    {ok, NewState :: term(), hibernate} |
    {swap_handler, Args1 :: term(), NewState :: term(),
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler.
-callback handle_call(Request :: term(), State :: term()) ->
    {ok, Reply :: term(), NewState :: term()} |
    {ok, Reply :: term(), NewState :: term(), hibernate} |
    {swap_handler, Reply :: term(), Args1 :: term(), NewState :: term(),
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    {remove_handler, Reply :: term()}.
-callback handle_info(Info :: term(), State :: term()) ->
    {ok, NewState :: term()} |
    {ok, NewState :: term(), hibernate} |
    {swap_handler, Args1 :: term(), NewState :: term(),
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler.
-callback terminate(Args :: (term() | {stop, Reason :: term()} |
                             stop | remove_handler |
                             {error, {'EXIT', Reason :: term()}} |
                             {error, term()}),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}),
                      State :: term(), Extra :: term()) ->
    {ok, NewState :: term()}.
```

### start/0,1;start_link/0,1
启动gen_event管理进程，调用gen:start/5,6和gen:start_link/5,6同步启动。start和start_link的区别在于是否跟启动进程link，参数可以指明gen_event进程的注册名。gen回调到init_it/6，设置trap_exit，给启动进程返回当前进程的pid，然后进入主循环loop/5，等待消息。start启动的gen_event进程的父进程为进程本身，start_link启动的为启动进程。
``` Erlang
start() ->
    gen:start(?MODULE, nolink, ?NO_CALLBACK, [], []).
start(Name) ->
    gen:start(?MODULE, nolink, Name, ?NO_CALLBACK, [], []).
start_link() ->
    gen:start(?MODULE, link, ?NO_CALLBACK, [], []).
start_link(Name) ->
    gen:start(?MODULE, link, Name, ?NO_CALLBACK, [], []).

init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, _, _, Options) ->
    process_flag(trap_exit, true),
    Debug = gen:debug_options(Options),
    proc_lib:init_ack(Starter, {ok, self()}),
    Name = name(Name0),
    loop(Parent, Name, [], Debug, false). % 此时handler为[]

%% handler
-record(handler, {
        module, % 回调模块
        id = false, % 同一回调模块不同的Id识别
        state,  % 状态
        supervised = false % sup handler
    }.
```

### add_handler/3;add_sup_handler/3
gen_event管理进程添加event handler。调用的是gen:call/4进行同步请求。其中第一个参数M是gen_event管理进程的pid或者是注册名，参数Handler为handler回调模块，参数Args是handler初始化时的参数，即传递给Mod:init/1是参数。其中Handler可以是{Mod, Id}，为了区别同一个回调模块的不同handler。gen_event不限制同名的handler，所以在添加时没有进行handler已有验证。add_handler和add_sup_handler的区别在于是否跟调用添加handler的进程link，同时记录在#handler.supervised中。这里回调Mod:init/1进行初始化工作，返回state，记录到#handler.state。
``` Erlang
add_handler(M, Handler, Args) -> rpc(M, {add_handler, Handler, Args}).

add_sup_handler(M, Handler, Args)  ->
    rpc(M, {add_sup_handler, Handler, Args, self()}).

rpc(M, Cmd) ->
    {ok, Reply} = gen:call(M, self(), Cmd, infinity),
    Reply.

handle_msg(Msg, Parent, ServerName, MSL, Debug) ->
    case Msg of
    {From, Tag, {add_handler, Handler, Args}} ->
        {Hib, Reply, MSL1} = server_add_handler(Handler, Args, MSL),
        ?reply(Reply),
        loop(Parent, ServerName, MSL1, Debug, Hib);
    {From, Tag, {add_sup_handler, Handler, Args, SupP}} ->
        {Hib, Reply, MSL1} = server_add_sup_handler(Handler, Args, MSL, SupP),
        ?reply(Reply),
        loop(Parent, ServerName, MSL1, Debug, Hib);

server_add_handler({Mod,Id}, Args, MSL) ->
    Handler = #handler{module = Mod,
               id = Id},
    server_add_handler(Mod, Handler, Args, MSL);
server_add_handler(Mod, Args, MSL) ->
    Handler = #handler{module = Mod},
    server_add_handler(Mod, Handler, Args, MSL).

server_add_handler(Mod, Handler, Args, MSL) ->
    case catch Mod:init(Args) of
        {ok, State} ->
        {false, ok, [Handler#handler{state = State}|MSL]};
        {ok, State, hibernate} ->
        {true, ok, [Handler#handler{state = State}|MSL]};
        Other ->
            {false, Other, MSL}
    end.

server_add_sup_handler({Mod,Id}, Args, MSL, Parent) ->
    link(Parent),
    Handler = #handler{module = Mod,
               id = Id,
               supervised = Parent},
    server_add_handler(Mod, Handler, Args, MSL);
server_add_sup_handler(Mod, Args, MSL, Parent) ->
    link(Parent),
    Handler = #handler{module = Mod,
               supervised = Parent},
    server_add_handler(Mod, Handler, Args, MSL).
```

### notify/2;sync_notify/2
给gen_event进程的event，也就是发送notify消息。notify为异步消息，sync_notify为同步消息。这里依次对所有的handler回调Mod:handler_event/2，更新handler的状态，如果Mod:handler_event/2返回remove_handler或者是执行出错了，则会handler会退出（调用terminate回调函数），然后从gen_event管理进程删除。
``` Erlang
notify(M, Event) -> send(M, {notify, Event}).

sync_notify(M, Event) -> rpc(M, {sync_notify, Event}).

send({global, Name}, Cmd) ->
    catch global:send(Name, Cmd),
    ok;
send({via, Mod, Name}, Cmd) ->
    catch Mod:send(Name, Cmd),
    ok;
send(M, Cmd) ->
    M ! Cmd,
    ok.

handle_msg(Msg, Parent, ServerName, MSL, Debug) ->
    case Msg of
    {notify, Event} ->
        {Hib,MSL1} = server_notify(Event, handle_event, MSL, ServerName),
        loop(Parent, ServerName, MSL1, Debug, Hib);
    {From, Tag, {sync_notify, Event}} ->
        {Hib, MSL1} = server_notify(Event, handle_event, MSL, ServerName),
        ?reply(ok),
        loop(Parent, ServerName, MSL1, Debug, Hib);

server_notify(Event, Func, [Handler|T], SName) ->
    case server_update(Handler, Func, Event, SName) of
    {ok, Handler1} ->
        {Hib, NewHandlers} = server_notify(Event, Func, T, SName),
        {Hib, [Handler1|NewHandlers]};
    {hibernate, Handler1} ->
        {_Hib, NewHandlers} = server_notify(Event, Func, T, SName),
        {true, [Handler1|NewHandlers]};
    no ->
        server_notify(Event, Func, T, SName)
    end;
server_notify(_, _, [], _) ->
    {false, []}.

server_update(Handler1, Func, Event, SName) ->
    Mod1 = Handler1#handler.module,
    State = Handler1#handler.state,
    case catch Mod1:Func(Event, State) of
    {ok, State1} ->
        {ok, Handler1#handler{state = State1}};
    {ok, State1, hibernate} ->
        {hibernate, Handler1#handler{state = State1}};
    {swap_handler, Args1, State1, Handler2, Args2} ->
        do_swap(Mod1, Handler1, Args1, State1, Handler2, Args2, SName);
    remove_handler ->
        do_terminate(Mod1, Handler1, remove_handler, State,
             remove, SName, normal),
        no;
    Other ->
        do_terminate(Mod1, Handler1, {error, Other}, State,
             Event, SName, crash),
        no
    end.
```

### delete_handler/2
从gen_event管理进程删除指定的handler。首先从所有的handler中找到最近添加的handler并从列表中删除，然后执行回调函数Mod:terminate/2进行退出工作。如果handler是通过add_sup_handler添加的，则删除时会给添加的进程发删除的通知{gen_event_EXIT, Handler, Reason}。
``` Erlang
delete_handler(M, Handler, Args) -> rpc(M, {delete_handler, Handler, Args}).

handle_msg(Msg, Parent, ServerName, MSL, Debug) ->
    case Msg of
    {From, Tag, {delete_handler, Handler, Args}} ->
        {Reply, MSL1} = server_delete_handler(Handler, Args, MSL, ServerName),
        ?reply(Reply),
        loop(Parent, ServerName, MSL1, Debug, false);

server_delete_handler(HandlerId, Args, MSL, SName) ->
    case split(HandlerId, MSL) of
    {Mod, Handler, MSL1} ->
        {do_terminate(Mod, Handler, Args,
              Handler#handler.state, delete, SName, normal),
         MSL1};
    error ->
        {{error, module_not_found}, MSL}
    end.

do_terminate(Mod, Handler, Args, State, LastIn, SName, Reason) ->
    Res = (catch Mod:terminate(Args, State)),
    report_terminate(Handler, Reason, Args, State, LastIn, SName, Res),
    Res.

report_terminate(Handler, crash, {error, Why}, State, LastIn, SName, _) ->
    report_terminate(Handler, Why, State, LastIn, SName);
report_terminate(Handler, How, _, State, LastIn, SName, _) ->
    %% How == normal | shutdown | {swapped, NewHandler, NewSupervisor}
    report_terminate(Handler, How, State, LastIn, SName).

report_terminate(Handler, Reason, State, LastIn, SName) ->
    report_error(Handler, Reason, State, LastIn, SName),
    case Handler#handler.supervised of
    false ->
        ok;
    Pid ->
        Pid ! {gen_event_EXIT,handler(Handler),Reason},
        ok
    end.
```

### swap_handler/3;swap_sup_handler/3
切换gen_event管理进程中的handler，用新的handler代替旧的handler。split_and_terminate/6找到旧的handler并从列表中删除，然后调用do_terminate/7结束旧的handler。然后通过server_add_handler/3或者server_add_sup_handler/4来添加新的handler。
``` Erlang
swap_handler(M, {H1, A1}, {H2, A2}) -> rpc(M, {swap_handler, H1, A1, H2, A2}).

swap_sup_handler(M, {H1, A1}, {H2, A2}) ->
    rpc(M, {swap_sup_handler, H1, A1, H2, A2, self()}).

handle_msg(Msg, Parent, ServerName, MSL, Debug) ->
    case Msg of
    {From, Tag, {swap_handler, Handler1, Args1, Handler2, Args2}} ->
        {Hib, Reply, MSL1} = server_swap_handler(Handler1, Args1, Handler2,
                             Args2, MSL, ServerName),
        ?reply(Reply),
        loop(Parent, ServerName, MSL1, Debug, Hib);
    {From, Tag, {swap_sup_handler, Handler1, Args1, Handler2, Args2,
             Sup}} ->
        {Hib, Reply, MSL1} = server_swap_handler(Handler1, Args1, Handler2,
                        Args2, MSL, Sup, ServerName),
        ?reply(Reply),
        loop(Parent, ServerName, MSL1, Debug, Hib);

server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, SName) ->
    {State2, Sup, MSL1} = split_and_terminate(Handler1, Args1, MSL,
                          SName, Handler2, false),
    case s_s_h(Sup, Handler2, {Args2, State2}, MSL1) of
    {Hib, ok, MSL2} ->
        {Hib, ok, MSL2};
    {Hib, What, MSL2} ->
        {Hib, {error, What}, MSL2}
    end.

server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, Sup, SName) ->
    {State2, _, MSL1} = split_and_terminate(Handler1, Args1, MSL,
                        SName, Handler2, Sup),
    case s_s_h(Sup, Handler2, {Args2, State2}, MSL1) of
    {Hib, ok, MSL2} ->
        {Hib, ok, MSL2};
    {Hib, What, MSL2} ->
        {Hib, {error, What}, MSL2}
    end.

split_and_terminate(HandlerId, Args, MSL, SName, Handler2, Sup) ->
    case split(HandlerId, MSL) of
    {Mod, Handler, MSL1} ->
        OldSup = Handler#handler.supervised,
        NewSup = if
             not Sup -> OldSup;
             true    -> Sup
             end,
        {do_terminate(Mod, Handler, Args,
              Handler#handler.state, swapped, SName,
              {swapped, Handler2, NewSup}),
         OldSup,
         MSL1};
    error ->
            {error, false, MSL}
    end.

s_s_h(false, Handler, Args, MSL) ->
    server_add_handler(Handler, Args, MSL);
s_s_h(Pid, Handler, Args, MSL) ->
    server_add_sup_handler(Handler, Args, MSL, Pid).
```

### call/3,4
gen_event特定handler的同步请求。首先搜索出最近添加的相应handler，然后执行回调函数Mod:handle_call/2返回新的状态，更新所有相应的handler。如果返回的是remove_handler或者执行出错了，则会删除所有相应的handler。
``` Erlang
call(M, Handler, Query) -> call1(M, Handler, Query).

call(M, Handler, Query, Timeout) -> call1(M, Handler, Query, Timeout).

call1(M, Handler, Query) ->
    Cmd = {call, Handler, Query},
    try gen:call(M, self(), Cmd) of
    {ok, Res} ->
        Res
    catch
    exit:Reason ->
        exit({Reason, {?MODULE, call, [M, Handler, Query]}})
    end.

call1(M, Handler, Query, Timeout) ->
    Cmd = {call, Handler, Query},
    try gen:call(M, self(), Cmd, Timeout) of
    {ok, Res} ->
        Res
    catch
    exit:Reason ->
        exit({Reason, {?MODULE, call, [M, Handler, Query, Timeout]}})
    end.

handle_msg(Msg, Parent, ServerName, MSL, Debug) ->
    case Msg of
    {From, Tag, {call, Handler, Query}} ->
        {Hib, Reply, MSL1} = server_call(Handler, Query, MSL, ServerName),
        ?reply(Reply),
        loop(Parent, ServerName, MSL1, Debug, Hib);

server_call(Handler, Query, MSL, SName) ->
    case search(Handler, MSL) of
    {ok, Ha} ->
        case server_call_update(Ha, Query, SName) of
        {no, Reply} ->
            {false, Reply, delete(Handler, MSL)};
        {{ok, Ha1}, Reply} ->
            {false, Reply, replace(Handler, MSL, Ha1)};
        {{hibernate, Ha1}, Reply} ->
            {true, Reply, replace(Handler, MSL, Ha1)}
        end;
    false ->
        {false, {error, bad_module}, MSL}
    end.

server_call_update(Handler1, Query, SName) ->
    Mod1 = Handler1#handler.module,
    State = Handler1#handler.state,
    case catch Mod1:handle_call(Query, State) of
    {ok, Reply, State1} ->
        {{ok, Handler1#handler{state = State1}}, Reply};
    {ok, Reply, State1, hibernate} ->
        {{hibernate, Handler1#handler{state = State1}},
         Reply};
    {swap_handler, Reply, Args1, State1, Handler2, Args2} ->
        {do_swap(Mod1,Handler1,Args1,State1,Handler2,Args2,SName), Reply};
    {remove_handler, Reply} ->
        do_terminate(Mod1, Handler1, remove_handler, State,
             remove, SName, normal),
        {no, Reply};
    Other ->
        do_terminate(Mod1, Handler1, {error, Other}, State,
             Query, SName, crash),
        {no, {error, Other}}
    end.
```

### 其他消息
除call和notify消息外，gen_event管理进程可以处理其他消息。比如which_handlers，get_modules消息等。其中link进程退出消息，可能是某个handler的supervised记录的进程，当收到这个消息时，此类型的handler会退出并删除，同时给其他handler发handle_info消息。而除此之外的其他消息会回调到各handler模块的Mod:handle_info/2函数。

``` Erlang
handle_msg(Msg, Parent, ServerName, MSL, Debug) ->
    case Msg of
    {'EXIT', From, Reason} ->
        MSL1 = handle_exit(From, Reason, MSL, ServerName),
        loop(Parent, ServerName, MSL1, Debug, false);
    {From, Tag, which_handlers} ->
        ?reply(the_handlers(MSL)),
        loop(Parent, ServerName, MSL, Debug, false);
    {From, Tag, get_modules} ->
        ?reply(get_modules(MSL)),
        loop(Parent, ServerName, MSL, Debug, false);
    Other  ->
        {Hib, MSL1} = server_notify(Other, handle_info, MSL, ServerName),
        loop(Parent, ServerName, MSL1, Debug, Hib)
    end.

handle_exit(From, Reason, MSL, SName) ->
    MSL1 = terminate_supervised(From, Reason, MSL, SName),
    {_,MSL2}=server_notify({'EXIT', From, Reason}, handle_info, MSL1, SName),                                                                                    MSL2.

terminate_supervised(Pid, Reason, MSL, SName) ->
    F = fun(Ha) when Ha#handler.supervised =:= Pid ->
        do_terminate(Ha#handler.module,
                Ha,
                {stop,Reason},
                Ha#handler.state,
                {parent_terminated, {Pid,Reason}},
                SName,
                shutdown),
        false;
    (_) ->
        true
    end,
    lists:filter(F, MSL).
```

### stop/1
结束gen_event管理进程，如果其父进程退出则gen_event同样会退出。依次结束所有的handler，同时unlink handler中的supervisor进程，不过如果此监督进程是父进程时不执行unlink。
``` Erlang
stop(M) -> rpc(M, stop).

handle_msg(Msg, Parent, ServerName, MSL, Debug) ->
    case Msg of
    {From, Tag, stop} ->
        catch terminate_server(normal, Parent, MSL, ServerName),
        ?reply(ok);

fetch_msg(Parent, ServerName, MSL, Debug, Hib) ->
    receive
    {'EXIT', Parent, Reason} ->
        terminate_server(Reason, Parent, MSL, ServerName);

terminate_server(Reason, Parent, MSL, ServerName) ->
    stop_handlers(MSL, ServerName),
    do_unlink(Parent, MSL),
    exit(Reason).

%% unlink the supervisor process of all supervised handlers.
%% We do not want a handler supervisor to EXIT due to the
%% termination of the event manager (server).
%% Do not unlink Parent !
do_unlink(Parent, MSL) ->
    lists:foreach(fun(Handler) when Handler#handler.supervised =:= Parent ->
              true;
             (Handler) when is_pid(Handler#handler.supervised) ->
              unlink(Handler#handler.supervised),
              true;
             (_) ->
              true
          end,
          MSL).

stop_handlers([Handler|T], SName) ->
    Mod = Handler#handler.module,
    do_terminate(Mod, Handler, stop, Handler#handler.state,
         stop, SName, shutdown),
    stop_handlers(T, SName);
stop_handlers([], _) ->                                                                                                                                          [].
```

### 总结
&emsp;&emsp;gen_event管理进程主动trap_exit，可以动态添加和删除handler。handler之间互不关联, 互不影响。删除handler时，不主动unlink其的supervisor进程（如果有的话），因为有可能其他的handler需要link同一个进程。
