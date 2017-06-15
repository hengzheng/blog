---
title: Erlang启动过程学习
date: 2017-05-26 15:16:29
tags: Erlang
category: Erlang
---

### 简介
&emsp;&emsp;本文学习Erlang启动过程中涉及到的Erlang源代码。学习环境为linux，Erlang/OTP代码版本是R16B03-1。erl实际上是个shell脚本，设置几个环境变量后，调用exec执行erlexec，代码在/erts/etc/common/erlexec.c中。erlexec分析erl传入的参数和环境变量，选择正确版本的beam(例如在smp环境中，运行的就是 beam.smp 版本的虚拟机)，然后将传入的参数整理好，加入一些默认参数（如-root,-progname,-home），最后通过系统调用execv运行beam虚拟机。执行erl时加上–emu_args选项可以打印出erl启动参数。beam进程入口是erts/emulator/sys/unix/erl_main.c，其中调用erl_start(argc, argv)，通过erl_start进入到Erlang虚拟机的世界了。erl_start函数在erts/emulator/beam/erl_init.c中，主要进行一些底层的初始化工作，如信号处理、初始化数据结构、启动调度器等，其中通过otp_ring0:start/2启动了Erlang虚拟机的第一个进程init。
<!--- more --->

### init
init:boot/1的代码如下：
``` Erlang
boot(BootArgs) ->
    register(init, self()),
    process_flag(trap_exit, true),
    start_on_load_handler_process(),
    {Start0,Flags,Args} = parse_boot_args(BootArgs),
    Start = map(fun prepare_run_args/1, Start0),
    Flags0 = flags_to_atoms_again(Flags),
    boot(Start,Flags0,Args).
```
首先将进程注册为init，然后start_on_load_handler_process/0启动了?ON_LOAD_HANDLER(即init__boot__on_load_handler)进程，记录一些在boot时加载但加载失败的模块，然后在kernel application启动时再次检测这些模块是否可用(默认的boot不加载模块)。代码如下：
``` Erlang
start_on_load_handler_process() ->
    register(?ON_LOAD_HANDLER,
         spawn(fun on_load_handler_init/0)).

on_load_handler_init() ->
    on_load_loop([], false).

on_load_loop(Mods, Debug0) ->
    receive
    {init_debug_flag,Debug} ->
        on_load_loop(Mods, Debug);
    {loaded,Mod} ->
        on_load_loop([Mod|Mods], Debug0);
    run_on_load ->
        run_on_load_handlers(Mods, Debug0),
        exit(on_load_done)
    end.

%% 在kernel启动时会调用 
run_on_load_handlers() -> 
    Ref = monitor(process, ?ON_LOAD_HANDLER), 
    catch ?ON_LOAD_HANDLER ! run_on_load, 
    receive 
    {'DOWN',Ref,process,_,noproc} -> 
        %% There is no on_load handler process, 
        %% probably because init:restart/0 has been 
        %% called and it is not the first time we 
        %% pass through here. 
        ok;  
    {'DOWN',Ref,process,_,on_load_done} -> 
        ok;  
    {'DOWN',Ref,process,_,Res} -> 
        %% Failure to run an on_load handler. 
        %% This is fatal during start-up. 
        exit(Res) 
    end. 
run_on_load_handlers([M|Ms], Debug) ->
    debug(Debug, {running_on_load_handler,M}),
    Fun = fun() ->
          Res = erlang:call_on_load_function(M),
          exit(Res)
      end,
    {Pid,Ref} = spawn_monitor(Fun),
    receive
    {'DOWN',Ref,process,Pid,OnLoadRes} ->
        Keep = OnLoadRes =:= ok,
        erlang:finish_after_on_load(M, Keep),
        case Keep of
        false ->
            Error = {on_load_function_failed,M},
            debug(Debug, Error),
            exit(Error);
        true ->
            debug(Debug, {on_load_handler_returned_ok,M}),
            run_on_load_handlers(Ms, Debug)
        end
    end;
run_on_load_handlers([], _) -> ok.
```

parse_boot_args/1解释参数，主要是-extra,-s,-run,-eval,-前缀的flags和其他参数。然后格式化参数（binary/list转换成atom），传递给boot/3，代码如下：
``` Erlang
boot(Start,Flags,Args) ->
    BootPid = do_boot(Flags,Start),
    State = #state{flags = Flags, 
           args = Args,
           start = Start,
           bootpid = BootPid},
    boot_loop(BootPid,State).

%% state结构
-record(state, {flags = [],
        args = [],
        start = [],
        kernel = []                   :: [{atom(), pid()}],
        bootpid                       :: pid(),
        status = {starting, starting} :: {internal_status(), term()},
        script_id = [],
        loaded = [],
        subscribed = []}).
```

do_boot/2启动新进程执行boot逻辑，然后设置state进入到boot_loop/2，等待和处理do_boot/2期间的消息。do_boot/2执行完后，最后init进入到loop/1主循环中。boot_loop/2和loop/1代码如下：
``` Erlang
boot_loop(BootPid, State) ->
    receive
    {BootPid,loaded,ModLoaded} ->
        Loaded = State#state.loaded,
        boot_loop(BootPid,State#state{loaded = [ModLoaded|Loaded]});
    {BootPid,started,KernelPid} ->
        boot_loop(BootPid, new_kernelpid(KernelPid, BootPid, State));
    {BootPid,progress,started} ->
            {InS,_} = State#state.status,
        notify(State#state.subscribed),
        boot_loop(BootPid,State#state{status = {InS,started},
                      subscribed = []});
    {BootPid,progress,NewStatus} ->
            {InS,_} = State#state.status,
        boot_loop(BootPid,State#state{status = {InS,NewStatus}});
    {BootPid,{script_id,Id}} ->
        boot_loop(BootPid,State#state{script_id = Id});
    {'EXIT',BootPid,normal} ->
            {_,PS} = State#state.status,
        notify(State#state.subscribed),
        loop(State#state{status = {started,PS},
                 subscribed = []});
    {'EXIT',BootPid,Reason} ->
        erlang:display({"init terminating in do_boot",Reason}),
        crash("init terminating in do_boot", [Reason]);
    {'EXIT',Pid,Reason} ->
        Kernel = State#state.kernel,
        terminate(Pid,Kernel,Reason), %% If Pid is a Kernel pid, halt()!
        boot_loop(BootPid,State);
    {stop,Reason} ->
        stop(Reason,State);
    {From,fetch_loaded} ->   %% Fetch and reset initially loaded modules.
        From ! {init,State#state.loaded},
        garb_boot_loop(BootPid,State#state{loaded = []});
    {From,{ensure_loaded,Module}} ->
        {Res, Loaded} = ensure_loaded(Module, State#state.loaded),
        From ! {init,Res},
        boot_loop(BootPid,State#state{loaded = Loaded});
    Msg ->
        boot_loop(BootPid,handle_msg(Msg,State))
    end.

%% Here is the main loop after the system has booted.
loop(State) ->
    receive
    {'EXIT',Pid,Reason} ->
        Kernel = State#state.kernel,
        terminate(Pid,Kernel,Reason), %% If Pid is a Kernel pid, halt()!
        loop(State);
    {stop,Reason} ->
        stop(Reason,State);
    {From,fetch_loaded} ->           %% The Loaded info is cleared in
        Loaded = State#state.loaded, %% boot_loop but is handled here 
        From ! {init,Loaded},        %% anyway.
        loop(State);
    {From, {ensure_loaded, _}} ->
        From ! {init, not_allowed},
        loop(State);
    Msg ->
        loop(handle_msg(Msg,State))
    end.
```

### boot
do_boot/2启动新进程进行引导逻辑。首先解释参数，比如-root，-loader，-path等。然后通过start_prim_loader/6启动主加载器进程erl_prim_loader，用来读取文件，也是第一个kernel级别（记录在init进程的#state.kernel）的进程。接着获取所使用的boot脚本文件，由-boot参数指定，默认$ROOT/bin/start.boot。eval_script/8执行boot文件一系列指令。如preLoaded，progress，path，primLoad，kernel_load_completed，kernelProcess（kernel级别进程）,apply等指令（具体可查看erl安装目录下的/usr/local/lib/erlang/bin/start.script）。

``` Erlang
%%% -------------------------------------------------
%%% The boot process fetches a boot script and loads
%%% all modules specified and starts spec. processes.
%%% Processes specified with -s or -run are finally started.
%%% -------------------------------------------------

do_boot(Flags,Start) ->
    Self = self(),
    spawn_link(fun() -> do_boot(Self,Flags,Start) end).

do_boot(Init,Flags,Start) ->
    process_flag(trap_exit,true),
    {Pgm0,Nodes,Id,Path} = prim_load_flags(Flags),
    Root = b2s(get_flag('-root',Flags)),
    PathFls = path_flags(Flags),
    Pgm = b2s(Pgm0),
    _Pid = start_prim_loader(Init,b2a(Id),Pgm,bs2as(Nodes),
                 bs2ss(Path),PathFls),
    BootFile = bootfile(Flags,Root),
    BootList = get_boot(BootFile,Root),
    LoadMode = b2a(get_flag('-mode',Flags,false)),
    Deb = b2a(get_flag('-init_debug',Flags,false)),
    catch ?ON_LOAD_HANDLER ! {init_debug_flag,Deb},
    BootVars = get_flag_args('-boot_var',Flags),
    ParallelLoad =
    (Pgm =:= "efile") and (erlang:system_info(thread_pool_size) > 0),

    PathChoice = code_path_choice(),
    eval_script(BootList,Init,PathFls,{Root,BootVars},Path,
        {true,LoadMode,ParallelLoad},Deb,PathChoice),

    %% To help identifying Purify windows that pop up,
    %% print the node name into the Purify log.
    (catch erlang:system_info({purify, "Node: " ++ atom_to_list(node())})),

    start_em(Start).

%% Eval a boot script.
%% Load modules and start processes.
%% If a start command does not spawn a new process the
%% boot process hangs (we want to ensure syncronicity).
%%
eval_script([{progress,Info}|CfgL],Init,PathFs,Vars,P,Ph,Deb,PathChoice) ->
    debug(Deb,{progress,Info}),
    init ! {self(),progress,Info},
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb,PathChoice);
eval_script([{preLoaded,_}|CfgL],Init,PathFs,Vars,P,Ph,Deb,PathChoice) ->
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb,PathChoice);
eval_script([{path,Path}|CfgL],Init,{Pa,Pz},Vars,false,Ph,Deb,PathChoice) ->
    RealPath0 = make_path(Pa, Pz, Path, Vars),
    RealPath = patch_path(RealPath0, PathChoice),
    erl_prim_loader:set_path(RealPath),
    eval_script(CfgL,Init,{Pa,Pz},Vars,false,Ph,Deb,PathChoice);
eval_script([{path,_}|CfgL],Init,PathFs,Vars,P,Ph,Deb,PathChoice) ->
    %% Ignore, use the command line -path flag.
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb,PathChoice);
eval_script([{kernel_load_completed}|CfgL],Init,PathFs,Vars,P,{_,embedded,Par},Deb,PathChoice) ->
    eval_script(CfgL,Init,PathFs,Vars,P,{true,embedded,Par},Deb,PathChoice);
eval_script([{kernel_load_completed}|CfgL],Init,PathFs,Vars,P,{_,E,Par},Deb,PathChoice) ->
    eval_script(CfgL,Init,PathFs,Vars,P,{false,E,Par},Deb,PathChoice);
eval_script([{primLoad,Mods}|CfgL],Init,PathFs,Vars,P,{true,E,Par},Deb,PathChoice)
  when is_list(Mods) ->
    if
    Par =:= true ->
        par_load_modules(Mods,Init);
    true ->
        load_modules(Mods)
    end,
    eval_script(CfgL,Init,PathFs,Vars,P,{true,E,Par},Deb,PathChoice);
eval_script([{primLoad,_Mods}|CfgL],Init,PathFs,Vars,P,{false,E,Par},Deb,PathChoice) ->
    %% Do not load now, code_server does that dynamically!
    eval_script(CfgL,Init,PathFs,Vars,P,{false,E,Par},Deb,PathChoice);
eval_script([{kernelProcess,Server,{Mod,Fun,Args}}|CfgL],Init,
        PathFs,Vars,P,Ph,Deb,PathChoice) ->
    debug(Deb,{start,Server}),
    start_in_kernel(Server,Mod,Fun,Args,Init),
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb,PathChoice);
eval_script([{apply,{Mod,Fun,Args}}|CfgL],Init,PathFs,Vars,P,Ph,Deb,PathChoice) ->
    debug(Deb,{apply,{Mod,Fun,Args}}),
    apply(Mod,Fun,Args),
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb,PathChoice);
eval_script([],_,_,_,_,_,_,_) ->
    ok;
eval_script(What,_,_,_,_,_,_,_) ->
    exit({'unexpected command in bootfile',What}).
```
eval_script/8中的primLoad指令是加载模块，这里需要加载的模块记录在init进程的#state.loaded，同时如果失败的模块会记录在?ON_LOAD_HANDLER进程中。最后调用start_em/1来执行-s参数的MFA函数或者-eval参数的语名。至此do_boot/2运行完毕，进程结束，跟其link的init进程将收到{'EXIT', BootPid, normal}，然后init进到loop/1主循环中，init进程启动完毕。

### start.boot
start.boot对应的脚本为对应目录下的start.script，其内容如下：
``` Erlang
%% script generated at {2017,1,6} {16,43,8}
{script,
    {"OTP  APN 181 01","R16B03-1"},
    [{preLoaded,
         [erl_prim_loader,erlang,erts_internal,init,otp_ring0,prim_eval,
          prim_file,prim_inet,prim_zip,zlib]},
     {progress,preloaded},
     {path,["$ROOT/lib/kernel/ebin","$ROOT/lib/stdlib/ebin"]},
     {primLoad,[error_handler]},
     {kernel_load_completed},
     {progress,kernel_load_completed},
     {path,["$ROOT/lib/kernel/ebin"]},
     {primLoad,
         [application,application_controller,application_master,
          application_starter,auth,code,code_server,disk_log,disk_log_1,
          disk_log_server,disk_log_sup,dist_ac,dist_util,erl_boot_server,
          erl_ddll,erl_distribution,erl_epmd,erl_reply,error_logger,
          erts_debug,file,file_io_server,file_server,gen_sctp,gen_tcp,gen_udp,
          global,global_group,global_search,group,heart,hipe_unified_loader,
          inet,inet6_sctp,inet6_tcp,inet6_tcp_dist,inet6_udp,inet_config,
          inet_db,inet_dns,inet_gethost_native,inet_hosts,inet_parse,inet_res,
          inet_sctp,inet_tcp,inet_tcp_dist,inet_udp,kernel,kernel_config,net,
          net_adm,net_kernel,os,pg2,ram_file,rpc,seq_trace,standard_error,
          user,user_drv,user_sup,wrap_log_reader]},
     {path,["$ROOT/lib/stdlib/ebin"]},
     {primLoad,
         [array,base64,beam_lib,binary,c,calendar,dets,dets_server,dets_sup,
          dets_utils,dets_v8,dets_v9,dict,digraph,digraph_utils,edlin,
          edlin_expand,epp,erl_bits,erl_compile,erl_eval,erl_expand_records,
          erl_internal,erl_lint,erl_parse,erl_posix_msg,erl_pp,erl_scan,
          erl_tar,error_logger_file_h,error_logger_tty_h,escript,ets,
          eval_bits,file_sorter,filelib,filename,gb_sets,gb_trees,gen,
          gen_event,gen_fsm,gen_server,io,io_lib,io_lib_format,io_lib_fread,
          io_lib_pretty,lib,lists,log_mf_h,math,ms_transform,orddict,ordsets,
          otp_internal,pg,pool,proc_lib,proplists,qlc,qlc_pt,queue,random,re,
          sets,shell,shell_default,slave,sofs,string,supervisor,
          supervisor_bridge,sys,timer,unicode,win32reg,zip]},
     {progress,modules_loaded},
     {path,["$ROOT/lib/kernel/ebin","$ROOT/lib/stdlib/ebin"]},
     {kernelProcess,heart,{heart,start,[]}},
     {kernelProcess,error_logger,{error_logger,start_link,[]}},
     {kernelProcess,application_controller,
         {application_controller,start,
             [{application,kernel,
                  [{description,"ERTS  CXC 138 10"},
                   {vsn,"2.16.4"},
                   {id,[]},
                   {modules,
                       [application,application_controller,application_master,
                        application_starter,auth,code,code_server,dist_util,
                        erl_boot_server,erl_distribution,erl_reply,
                        error_handler,error_logger,file,file_server,
                        file_io_server,global,global_group,global_search,
                        group,heart,hipe_unified_loader,inet6_tcp,
                        inet6_tcp_dist,inet6_udp,inet6_sctp,inet_config,
                        inet_hosts,inet_gethost_native,inet_tcp_dist,kernel,
                        kernel_config,net,net_adm,net_kernel,os,ram_file,rpc,
                        user,user_drv,user_sup,disk_log,disk_log_1,
                        disk_log_server,disk_log_sup,dist_ac,erl_ddll,
                        erl_epmd,erts_debug,gen_tcp,gen_udp,gen_sctp,inet,
                        inet_db,inet_dns,inet_parse,inet_res,inet_tcp,
                        inet_udp,inet_sctp,pg2,seq_trace,standard_error,
                        wrap_log_reader]},
                   {registered,
                       [application_controller,erl_reply,auth,boot_server,
                        code_server,disk_log_server,disk_log_sup,
                        erl_prim_loader,error_logger,file_server_2,
                        fixtable_server,global_group,global_name_server,heart,
                        init,kernel_config,kernel_sup,net_kernel,net_sup,rex,
                        user,os_server,ddll_server,erl_epmd,inet_db,pg2]},
                   {applications,[]},
                   {included_applications,[]},
                   {env,[{error_logger,tty}]},
                   {maxT,infinity},
                   {maxP,infinity},
                   {mod,{kernel,[]}}]}]}},
     {progress,init_kernel_started},
     {apply,
         {application,load,
             [{application,stdlib,
                  [{description,"ERTS  CXC 138 10"},
                   {vsn,"1.19.4"},
                   {id,[]},
                   {modules,
                       [array,base64,beam_lib,binary,c,calendar,dets,
                        dets_server,dets_sup,dets_utils,dets_v8,dets_v9,dict,
                        digraph,digraph_utils,edlin,edlin_expand,epp,
                        eval_bits,erl_bits,erl_compile,erl_eval,
                        erl_expand_records,erl_internal,erl_lint,erl_parse,
                        erl_posix_msg,erl_pp,erl_scan,erl_tar,
                        error_logger_file_h,error_logger_tty_h,escript,ets,
                        file_sorter,filelib,filename,gb_trees,gb_sets,gen,
                        gen_event,gen_fsm,gen_server,io,io_lib,io_lib_format,
                        io_lib_fread,io_lib_pretty,lib,lists,log_mf_h,math,
                        ms_transform,orddict,ordsets,otp_internal,pg,pool,
                        proc_lib,proplists,qlc,qlc_pt,queue,random,re,sets,
                        shell,shell_default,slave,sofs,string,supervisor,
                        supervisor_bridge,sys,timer,unicode,win32reg,zip]},
                   {registered,
                       [timer_server,rsh_starter,take_over_monitor,
                        pool_master,dets]},
                   {applications,[kernel]},
                   {included_applications,[]},
                   {env,[]},
                   {maxT,infinity},
                   {maxP,infinity}]}]}},
     {progress,applications_loaded},
     {apply,{application,start_boot,[kernel,permanent]}},
     {apply,{application,start_boot,[stdlib,permanent]}},
     {apply,{c,erlangrc,[]}},
     {progress,started}]}.
```
在boot脚本中会启动一些kernel级别的进程，其中包括heart，error_logger，applicatio_controller(启动的同时加载kernel的app配置)，然后加载stdlib的app配置，启动kernel，stdlib两个application，所以最小的Erlang系统只包含kernel和stdlib。

### stop
init:stop/0是通过给init进程发{stop,stop}消息来关闭init进程。首先clear_system/2关闭所有的Processes和Ports，然后通过do_stop/2来选择是restart、reboot还是stop。代码如下：
``` Erlang
%%% -------------------------------------------------
%%% Stop the system. 
%%% Reason is: restart | reboot | stop
%%% According to reason terminate emulator or restart
%%% system using the same init process again.
%%% -------------------------------------------------

stop(Reason,State) ->
    BootPid = State#state.bootpid,
    {_,Progress} = State#state.status,
    State1 = State#state{status = {stopping, Progress}},
    clear_system(BootPid,State1),
    do_stop(Reason,State1).

do_stop(restart,#state{start = Start, flags = Flags, args = Args}) ->
    boot(Start,Flags,Args);
do_stop(reboot,_) ->
    halt();
do_stop(stop,State) ->
    stop_heart(State),
    halt();
do_stop({stop,Status},State) -> 
    stop_heart(State),
    halt(Status).

clear_system(BootPid,State) ->
    Heart = get_heart(State#state.kernel),
    shutdown_pids(Heart,BootPid,State),
    unload(Heart).

shutdown_pids(Heart,BootPid,State) ->
    Timer = shutdown_timer(State#state.flags),
    catch shutdown(State#state.kernel,BootPid,Timer,State),
    kill_all_pids(Heart), % Even the shutdown timer.
    kill_all_ports(Heart),
    flush_timout(Timer).

shutdown([{heart,_Pid}|Kernel],BootPid,Timer,State) ->
    shutdown(Kernel, BootPid, Timer, State);
shutdown([{_Name,Pid}|Kernel],BootPid,Timer,State) ->
    shutdown_kernel_pid(Pid, BootPid, Timer, State),
    shutdown(Kernel,BootPid,Timer,State);
shutdown(_,_,_,_) ->
    true.


%%
%% A kernel pid must handle the special case message
%% {'EXIT',Parent,Reason} and terminate upon it!
%%
shutdown_kernel_pid(Pid, BootPid, Timer, State) ->
    Pid ! {'EXIT',BootPid,shutdown},
    shutdown_loop(Pid, Timer, State, []).

%%
%% We have to handle init requests here in case a process
%% performs such a request and cannot shutdown (deadlock).
%% Keep all other EXIT messages in case it was another
%% kernel process. Resend these messages and handle later.
%%
shutdown_loop(Pid,Timer,State,Exits) ->
    receive
    {'EXIT',Pid,_} ->
        resend(reverse(Exits)),
        ok;
    {Timer,timeout} ->
        erlang:display({init,shutdown_timeout}),
        throw(timeout);
    {stop,_} ->
        shutdown_loop(Pid,Timer,State,Exits);
    {From,fetch_loaded} ->
        From ! {init,State#state.loaded},
        shutdown_loop(Pid,Timer,State,Exits);
    {'EXIT',OtherP,Reason} ->
        shutdown_loop(Pid,Timer,State,
              [{'EXIT',OtherP,Reason}|Exits]);
    Msg ->
        State1 = handle_msg(Msg,State),
        shutdown_loop(Pid,Timer,State1,Exits)
    end.
```
clear_system/2首先关闭记录在#state.kernel中的进程，shutdown/4中发送{'EXIT', BootPid, shutdown}消息来关闭kernel进程。此时是同步关闭的，init进程发送消息后，进入shutdown_loop/4通过receive等待kernel进程退出的消息。这个时候init还需要处理其他消息，如果是{'EXIT', OtherPid, Reason}消息则需要记录，等这个kernel进程关闭后，重新发给init自己，因为这里的receive会把之前收到的消息从消息队列中移除。#state.kernel中的进程全部关闭完后，关闭系统中除heart和init本身的所有进程，通过发送{'EXIT', Pid, kill}消息的方式，然后是关闭所有的port。最后通过unload/1释放所有已加载的模块（不包含预加载的模块，因为有可能restart init）。
``` Erlang
%% Kill all existing pids in the system (except init and heart).
kill_all_pids(Heart) ->
    case get_pids(Heart) of
    [] ->
        ok;
    Pids ->
        kill_em(Pids),
        kill_all_pids(Heart)  % Continue until all are really killed.
    end.
    
%% All except zombies.
alive_processes() ->
    [P || P <- processes(), erlang:is_process_alive(P)].

get_pids(Heart) ->
    Pids = alive_processes(),
    delete(Heart,self(),Pids).

delete(Heart,Init,[Heart|Pids]) -> delete(Heart,Init,Pids);
delete(Heart,Init,[Init|Pids])  -> delete(Heart,Init,Pids);
delete(Heart,Init,[Pid|Pids])   -> [Pid|delete(Heart,Init,Pids)];
delete(_,_,[])                  -> [].

kill_em([Pid|Pids]) ->
    exit(Pid,kill),
    kill_em(Pids);
kill_em([]) ->
    ok.

%%
%% Kill all existing ports in the system (except the heart port),
%% i.e. ports still existing after all processes have been killed.
%%
%% System ports like the async driver port will nowadays be immortal;
%% therefore, it is ok to send them exit signals...
%%
kill_all_ports(Heart) ->
    kill_all_ports(Heart,erlang:ports()).

kill_all_ports(Heart,[P|Ps]) ->
    case erlang:port_info(P,connected) of
    {connected,Heart} ->
        kill_all_ports(Heart,Ps);
    _ ->
        exit(P,kill),
        kill_all_ports(Heart,Ps)
    end;
kill_all_ports(_,_) ->
    ok.

unload(false) ->
    do_unload(sub(erlang:pre_loaded(),erlang:loaded()));
unload(_) ->
    do_unload(sub([heart|erlang:pre_loaded()],erlang:loaded())).

do_unload([M|Mods]) ->
    catch erlang:purge_module(M),
    catch erlang:delete_module(M),
    catch erlang:purge_module(M),
    do_unload(Mods);
do_unload([]) ->
    purge_all_hipe_refs(),
    ok.

purge_all_hipe_refs() ->
    case erlang:system_info(hipe_architecture) of
    undefined -> ok;
    _ -> hipe_bifs:remove_refs_from(all)
    end.
```

### 总结
&emsp;&emsp;Erlang启动由erl开始通过execerl加载BEAM虚拟机，然后创建init进程来进行一系列的引导逻辑。kernel和stdlib application是Erlang虚拟机最小组成部分。Erlang进程分为kernel级别进程，记录在init的#state.kernel中，关闭消息的Reason为shutdown，其他普通进程为kill。

### 参考
[Erlang启动过程分析](http://www.cnblogs.com/zhengsyao/archive/2012/08/15/Erlang-otp_start_up.html)
