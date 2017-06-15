---
title: Erlang源码学习--application_controller
date: 2017-05-31 10:27:45
tags: Erlang
category: Erlang
---

### 简介
&emsp;&emsp;Erlang源码学习环境为linux平台，Erlang/OTP代码版本是R16B03-1。本文主要学习application_controller的启动和结束过程相关代码。application表示一个实现了某些特定功能的组件，它可以作为单元来启动和关闭，也可以很好地被其他系统重用。application_controller为application提交相关接口，管理application，是在Erlang虚拟机启动时就启动的kernel进程。application_controller只管理local application，如果是distributed applications则由另一个叫dist_ac的进程管理。

<!--- more --->

### start/1
启动application_controller进程，参数是kernel application的配置数据。这里的配置数据只能是kernel application，因为kernel启动user进程（特殊之处在于user是application_controller进程的group_leader）。此进程由erlang虚拟机第一个进程init的do_load/3函数启动，所以其Parent PID是init。这里新创建了个进程执行init/2函数，然后通过receive来等待应答。init/2中首先注册进程为application_controller，check_config/0检测启动Erlang虚拟机时由-config参数传递的config文件，如果是sys.config文件，可以包含其他的.config文件。check_config_data/1检查.config文件相关配置内容。然后创建名为ac_tab的ets表，这里注释提到可以不再需要ac_tab表了，因为现在确保了在application启动时可以call application_controller进程来获取信息，而不一定要通过ac_tab表。make_appl/1分析和检查application配置，load/2加载application配置数据，同时加载配置中included_applications中包含的application配置数据，在ac_tab表中插入已加载application的#appl{}数据。load/2中传递了#state{}，源码这里实质上并没有改变状态。application配置数据加载成功后，给init进程发送ack应答消息，进入到gen_server主循环(所以application_controller也是gen_server行为的进程)，这时application_controller就启动完毕了。application_controller启动的同时加载了kernel的配置，同时加载了included_applications包含的其他application配置，但是也只是加载了配置，相关的application并没有启动。

``` Erlang
start(KernelApp) ->
    %% OTP-5811 Don't start as a gen_server to prevent crash report
    %% when (if) the process terminates
    Init = self(),
    AC = spawn_link(fun() -> init(Init, KernelApp) end),
    receive
    {ack, AC, ok} ->
        {ok, AC}; 
    {ack, AC, {error, Reason}} ->
        to_string(Reason); % init doesn't want error tuple, only a reason
    {'EXIT', _Pid, Reason} ->
        to_string(Reason)
    end. 

init(Init, Kernel) ->
    register(?AC, self()),
    process_flag(trap_exit, true),
    put('$ancestors', [Init]), % OTP-5811, for gen_server compatibility
    put('$initial_call', {application_controller, start, 1}),

    case catch check_conf() of
    {ok, ConfData} ->
        %% Actually, we don't need this info in an ets table anymore.
        %% This table was introduced because starting applications
        %% should be able to get som info from AC (e.g. loaded_apps).
        %% The new implementation makes sure the AC process can be
        %% called during start-up of any app.
        case check_conf_data(ConfData) of
        ok ->
            ets:new(ac_tab, [set, public, named_table]),
            S = #state{conf_data = ConfData},
            {ok, KAppl} = make_appl(Kernel),
            case catch load(S, KAppl) of
            {'EXIT', LoadError} ->
                Reason = {'load error', LoadError},
                Init ! {ack, self(), {error, to_string(Reason)}};
                        {error, Error} ->
                            Init ! {ack, self(), {error, to_string(Error)}};
            {ok, NewS} ->
                Init ! {ack, self(), ok},
                gen_server:enter_loop(?MODULE, [], NewS,
                          {local, ?AC})
            end;
        {error, ErrorStr} ->
            Str = lists:flatten(io_lib:format("invalid config data: ~ts", [ErrorStr])),
            Init ! {ack, self(), {error, to_string(Str)}}
        end;
    {error, {File, Line, Str}} ->
        ReasonStr =
        lists:flatten(io_lib:format("error in config file "
                        "~tp (~w): ~ts",
                        [File, Line, Str])),
        Init ! {ack, self(), {error, to_string(ReasonStr)}}
    end.

%% application_controller的state
-record(state, {loading = [], starting = [], start_p_false = [], running = [],
        control = [], started = [], start_req = [], conf_data}).

%% application配置的数据
-record(appl_data, {name, regs = [], phases, mod, mods = [],
                    inc_apps, maxP = infinity, maxT = infinity}).

%% application数据
-record(appl, {name, appl_data = #appl_data{}, descr, id, vsn, restart_type, inc_apps, apps}).
```

### terminate/2
application_controller是gen_server行为的进程，所以结束时执行的是terminate/2。首先如果设置了kernel application的shutdown_func的回调函数话，则先回调。然后取出kernel application设置的shutdown_timeout超时时间，默认为infinity。接着关闭记录在#state.running中的application_master进程，先发送{'EXIT', ACPid, shutdown}，如果超时了则发送{'EXIT', ACPid, kill}消息。最后删除名ac_tab的ets表。

``` Erlang
terminate(Reason, S) ->
    case application:get_env(kernel, shutdown_func) of
    {ok, {M, F}} ->
        catch M:F(Reason);
    _ ->                         
        ok    
    end,  
    ShutdownTimeout =  
    case application:get_env(kernel, shutdown_timeout) of
        undefined -> infinity;
        {ok,T} -> T
    end,
    foreach(fun({_AppName, Id}) when is_pid(Id) ->
            Ref = erlang:monitor(process, Id),
            unlink(Id),
            exit(Id, shutdown),
            receive
            %% Proc died before link
            {'EXIT', Id, _} -> ok
            after 0 ->
                receive
                {'DOWN', Ref, process, Id, _} -> ok
                after ShutdownTimeout ->
                    exit(Id, kill),
                    receive
                    {'DOWN', Ref, process, Id, _} -> ok
                    end
                end
            end;
           (_) -> ok
        end,
        S#state.running),
    true = ets:delete(ac_tab),
    ok.
```

### load_application/1，unload_application/1
加载和卸载application配置数据。加载的参数可以application名字，也可以是直接配置数据格式 ，如果是application名字，则先在code:get_path()路径下查找applicationName.app文件。do_load_application/2函数首先检测是否加载过，然后make_appl/1分析和检查application配置，load/2加载application配置数据，同时加载配置中included_applications中包含的application配置数据，在ac_tab表中插入已加载application的#appl{}数据。卸载的参数是application名字，首先检查application是否还在运行中，运行中不允许卸载，然后检查是否加载过。unload/2删除application相关的环境变量配置，删除ac_tab表中的相关数据，同时卸载included_applications中包含的application。

``` Erlang
load_application(Application) ->
    gen_server:call(?AC, {load_application, Application}, infinity).
    
unload_application(AppName) ->
    gen_server:call(?AC, {unload_application, AppName}, infinity).

%% 加载数据
handle_call({load_application, Application}, From, S) ->
    case catch do_load_application(Application, S) of
    {ok, NewS} ->
        AppName = get_appl_name(Application),
        case cntrl(AppName, S, {ac_load_application_req, AppName}) of
        true ->
            {noreply, S#state{loading = [{AppName, From} |
                         S#state.loading]}};
        false ->
            {reply, ok, NewS}
        end;
    {error, _} = Error ->
        {reply, Error, S};
    {'EXIT', R} ->
        {reply, {error, R}, S}
    end;
do_load_application(Application, S) ->
    case get_loaded(Application) of
    {true, _} ->  
        throw({error, {already_loaded, Application}});
    false ->  
        case make_appl(Application) of
        {ok, Appl} -> load(S, Appl);
        Error -> Error
        end
    end. 
load(S, {ApplData, ApplEnv, IncApps, Descr, Id, Vsn, Apps}) ->
    Name = ApplData#appl_data.name,
    ConfEnv = get_env_i(Name, S),
    NewEnv = merge_app_env(ApplEnv, ConfEnv),
    CmdLineEnv = get_cmd_env(Name),
    NewEnv2 = merge_app_env(NewEnv, CmdLineEnv),
    NewEnv3 = keyreplaceadd(included_applications, 1, NewEnv2,
                {included_applications, IncApps}),
    add_env(Name, NewEnv3),
    Appl = #appl{name = Name, descr = Descr, id = Id, vsn = Vsn,
         appl_data = ApplData, inc_apps = IncApps, apps = Apps},
    ets:insert(ac_tab, {{loaded, Name}, Appl}),
    NewS =
    foldl(fun(App, S1) ->
              case get_loaded(App) of
              {true, _} -> S1;
              false ->
                  case do_load_application(App, S1) of
                  {ok, S2} -> S2;
                  Error -> throw(Error)
                  end
              end
          end, S, IncApps),
    {ok, NewS}.

%% 卸载数据
handle_call({unload_application, AppName}, _From, S) -> 
    case lists:keymember(AppName, 1, S#state.running) of 
    true -> {reply, {error, {running, AppName}}, S}; 
    false -> 
        case get_loaded(AppName) of 
        {true, _} -> 
            NewS = unload(AppName, S), 
            cntrl(AppName, S, {ac_application_unloaded, AppName}), 
            {reply, ok, NewS}; 
        false -> 
            {reply, {error, {not_loaded, AppName}}, S} 
        end 
    end;
unload(AppName, S) ->
    {ok, IncApps} = get_env(AppName, included_applications),
    del_env(AppName),
    ets:delete(ac_tab, {loaded, AppName}),
    foldl(fun(App, S1) ->
          case get_loaded(App) of
              false -> S1;
              {true, _} -> unload(App, S1)
          end 
      end, S, IncApps).
```

### start_application/2
开启application_master进程。RestartType指明当application出错关闭时要做些什么，有三种类型分别是permanent、transient、temporary。permanent类型的application出错关闭时，所有其他的application也会关闭，然后application_controller也关闭，kernel和stdlib就是这种类型的；transient类型的application normal关闭时，不会影响其他的application，如果是非正常出错关闭的，则行为跟permanent一样；temporary类型的application出错关闭时，不会影响其他的application。如果是调用application_controller:stop/1关闭的，不受RestartType影响，也即是不会影响其他的application。
application_controller收到start_application消息，首先在请求列表（记录在#state.start_req）中检查是否有相同的请求，check_start_cond/4检查启动类型，是否已在运行中(记录在#state.running)和检查配置数据中applications字段指明的apps是否已启动(记录在#state.started)。检查是否有权限运行，然后通过swap_starter/4启动master进程，同时记录在#state.starting和#state.start_req中。swap_starter/4创建新的进程来执行init_starter/4，init_starter/4把start_appl/3运行结果通过gen_server:cast/1反馈给application_controller进程。start_appl/3检查配置数据中applications字段指明的apps是否已运行(记录在#state.running中)，然后调用application_master:start_link/2启动master，返回master的pid。
application_controller进程收到application_started消息后，首先给请求启动application的进程(记录在#state.start_req中)回馈结果，并从队列中删除。如果启动的是kernel application则check_user/0检查user进程，然后设置application_controller进程的group_leader为user。在#state.running和#state.started中记录，同时删除#state.starting中的信息，更新state。代码中也可以看到如果启动类型是permanent或者transient且失败原因是非normal的，则application_controller会关闭。

``` Erlang
start_application(AppName, RestartType) ->
    gen_server:call(?AC, {start_application, AppName, RestartType}, infinity).
handle_call({start_application, AppName, RestartType}, From, S) ->
    #state{running = Running, starting = Starting, start_p_false = SPF,
       started = Started, start_req = Start_req} = S,
    %% Check if the commandline environment variables are OK.
    %% Incase of erroneous variables do not start the application,
    %% if the application is permanent crash the node.
    %% Check if the application is already starting.
    case lists:keyfind(AppName, 1, Start_req) of
    false ->
        case catch check_start_cond(AppName, RestartType, Started, Running) of
        {ok, Appl} ->
            Cntrl = cntrl(AppName, S, {ac_start_application_req, AppName}),
            Perm = application:get_env(kernel, permissions),
            case {Cntrl, Perm} of
            {true, _} ->
                {noreply, S#state{starting = [{AppName, RestartType, normal, From} |
                              Starting],
                          start_req = [{AppName, From} | Start_req]}};
            {false, undefined} ->
                spawn_starter(From, Appl, S, normal),
                {noreply, S#state{starting = [{AppName, RestartType, normal, From} |
                              Starting],
                          start_req = [{AppName, From} | Start_req]}};
            {false, {ok, Perms}} ->
                case lists:member({AppName, false}, Perms) of
                false ->
                    spawn_starter(From, Appl, S, normal),
                    {noreply, S#state{starting = [{AppName, RestartType, normal, From} |
                                  Starting],
                              start_req = [{AppName, From} | Start_req]}};
                true ->
                    SS = S#state{start_p_false = [{AppName, RestartType, normal, From} |
                                  SPF]},
                    {reply, ok, SS}
                end
            end;
        {error, _R} = Error ->
            {reply, Error, S}
        end;
    {AppName, _FromX} ->
        SS = S#state{start_req = [{AppName, From} | Start_req]},
        {noreply, SS}
    end;

spawn_starter(From, Appl, S, Type) ->
    spawn_link(?MODULE, init_starter, [From, Appl, S, Type]).

init_starter(_From, Appl, S, Type) ->
    process_flag(trap_exit, true),
    AppName = Appl#appl.name,
    gen_server:cast(?AC, {application_started, AppName, 
              catch start_appl(Appl, S, Type)}).

start_appl(Appl, S, Type) ->
    ApplData = Appl#appl.appl_data,
    case ApplData#appl_data.mod of
    [] ->
        {ok, undefined};
    _ -> 
        %% Name = ApplData#appl_data.name,
        Running = S#state.running,
        foreach(
          fun(AppName) ->
              case lists:keymember(AppName, 1, Running) of
              true ->
                  ok;
              false ->
                  throw({info, {not_running, AppName}})
              end
          end, Appl#appl.apps),
        case application_master:start_link(ApplData, Type) of
        {ok, _Pid} = Ok ->
            Ok;
        {error, _Reason} = Error ->
            throw(Error)
        end
    end.

handle_cast({application_started, AppName, Res}, S) ->
    handle_application_started(AppName, Res, S).

handle_application_started(AppName, Res, S) ->
    #state{starting = Starting, running = Running, started = Started,
       start_req = Start_req} = S,
    Start_reqN = reply_to_requester(AppName, Start_req, Res),
    {_AppName, RestartType, _Type, _From} = lists:keyfind(AppName, 1, Starting),
    case Res of
    {ok, Id} ->
        case AppName of
        kernel -> check_user();
        _ -> ok
        end,
        info_started(AppName, nd(Id)),
        notify_cntrl_started(AppName, Id, S, ok),
        NRunning = keyreplaceadd(AppName, 1, Running,{AppName,Id}),
        NStarted = keyreplaceadd(AppName, 1, Started,{AppName,RestartType}),
        NewS =  S#state{starting = keydelete(AppName, 1, Starting),
                running = NRunning,
                started = NStarted,
                start_req = Start_reqN},
        %% The permission may have been changed during start
        Perm = application:get_env(kernel, permissions),
        case {Perm, Id} of
        {undefined, _} ->
            {noreply, NewS};
        %% Check only if the application is started on the own node
        {{ok, Perms}, {distributed, StartNode}} when StartNode =:= node() ->
            case lists:member({AppName, false}, Perms) of
            true ->
                #state{running = StopRunning, started = StopStarted} = NewS,
                case lists:keyfind(AppName, 1, StopRunning) of
                {_AppName, Id} ->
                    {_AppName2, Type} =
                    lists:keyfind(AppName, 1, StopStarted),
                    stop_appl(AppName, Id, Type),
                    NStopRunning = keydelete(AppName, 1, StopRunning),
                    cntrl(AppName, NewS, {ac_application_stopped, AppName}),
                    {noreply, NewS#state{running = NStopRunning,
                            started = StopStarted}};
                false ->
                    {noreply, NewS}
                end;
            false ->
                {noreply, NewS}
            end;
        _ ->
            {noreply, NewS}
        end;
    {error, R} = Error when RestartType =:= temporary ->
        notify_cntrl_started(AppName, undefined, S, Error),
        info_exited(AppName, R, RestartType),
        {noreply, S#state{starting = keydelete(AppName, 1, Starting),
                  start_req = Start_reqN}};
    {info, R} when RestartType =:= temporary ->
        notify_cntrl_started(AppName, undefined, S, {error, R}),
        {noreply, S#state{starting = keydelete(AppName, 1, Starting),
                  start_req = Start_reqN}};
    {ErrInf, R} when RestartType =:= transient, ErrInf =:= error;
             RestartType =:= transient, ErrInf =:= info ->
        notify_cntrl_started(AppName, undefined, S, {error, R}),
        case ErrInf of
        error ->
            info_exited(AppName, R, RestartType);
        info ->
            ok
        end,
        case R of
        {{'EXIT',normal},_Call} ->
            {noreply, S#state{starting = keydelete(AppName, 1, Starting),
                      start_req = Start_reqN}};
        _ ->
            Reason = {application_start_failure, AppName, R},
            {stop, to_string(Reason), S}
        end;
    {error, R} = Error -> %% permanent
        notify_cntrl_started(AppName, undefined, S, Error),
        info_exited(AppName, R, RestartType),
        Reason = {application_start_failure, AppName, R},
        {stop, to_string(Reason), S};
    {info, R} -> %% permanent
        notify_cntrl_started(AppName, undefined, S, {error, R}),
        Reason = {application_start_failure, AppName, R},
        {stop, to_string(Reason), S}
    end.
```

### stop_application/1
application_controller收到stop_application消息后，首先检查application是否运行中(记录在#state.running中)，如果没有则检查是否已启动(记录在#state.started中)。stop_appl/3关闭application_master进程，然后删除#state.running和#state.started中的记录，更新state。调用这个函数来关闭application，不会影响到其他的app。

``` Erlang
stop_application(AppName) ->
    gen_server:call(?AC, {stop_application, AppName}, infinity).
handle_call({stop_application, AppName}, _From, S) ->
    #state{running = Running, started = Started} = S,
    case lists:keyfind(AppName, 1, Running) of
    {_AppName, Id} ->
        {_AppName2, Type} = lists:keyfind(AppName, 1, Started),
        stop_appl(AppName, Id, Type),
        NRunning = keydelete(AppName, 1, Running),
        NStarted = keydelete(AppName, 1, Started),
        cntrl(AppName, S, {ac_application_stopped, AppName}),
        {reply, ok, S#state{running = NRunning, started = NStarted}};
    false ->
        case lists:keymember(AppName, 1, Started) of
        true ->
            NStarted = keydelete(AppName, 1, Started),
            cntrl(AppName, S, {ac_application_stopped, AppName}),
            {reply, ok, S#state{started = NStarted}};
        false ->
            {reply, {error, {not_started, AppName}}, S}
        end
    end;

%%-----------------------------------------------------------------
%% Stop application locally.
%%-----------------------------------------------------------------
stop_appl(AppName, Id, Type) when is_pid(Id) ->
    unlink(Id),
    application_master:stop(Id),
    info_exited(AppName, stopped, Type),
    ets:delete(ac_tab, {application_master, AppName});
stop_appl(AppName, undefined, Type) ->
    %% Code-only application stopped
    info_exited(AppName, stopped, Type);
stop_appl(_AppName, _Id, _Type) ->
    %% Distributed application stopped
    ok.
```

### 总结
&emsp;&emsp;application_controller由init进程启动，启动时加载了kernel application的配置数据，在启动kernel application后group_leader由init变为user。application_controller是gen_server行为的进程，为application模块提供相关接口，统一管理application，而每个application又由application_master进程管理。
