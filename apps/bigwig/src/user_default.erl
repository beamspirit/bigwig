-module(user_default).
-compile(export_all).
-import(io, [format/1]). 

super_restart(Sup,Mod) ->
    io:format("Terminating ~p:~p~n", [Sup,Mod]),
    A = supervisor:terminate_child(Sup, Mod),
    io:format("Restarting ~p:~p~n", [Sup,Mod]),
    B = supervisor:restart_child(Sup, Mod),
    {A,B}.

help() -> 
    shell_default:help(),
    format("super_restart(Sup,Id)   -- restart a child on a running supervisor\n"),
    dbghelp().

dbghelp() ->    
    format("dbgtc(File)             -- use dbg:trace_client() to read data from File\n"), 
    format("dbgon(M)                -- enable dbg tracer on all funs in module M\n"), 
    format("dbgon(M,Fun)            -- enable dbg tracer for module M and function F\n"), 
    format("dbgon(M,File)           -- enable dbg tracer for module M and log to File\n"), 
    format("dbgadd(M)               -- enable call tracer for module M\n"), 
    format("dbgadd(M,F)             -- enable call tracer for function M:F\n"), 
    format("dbgdel(M)               -- disable call tracer for module M\n"), 
    format("dbgdel(M,F)             -- disable call tracer for function M:F\n"), 
    format("dbgoff()                -- disable dbg tracer (calls dbg:stop/0)\n"), 
    format("l()                     -- load all changed modules\n"), 
    format("nl()                    -- load all changed modules on all known nodes\n"), 
    format("mm()                    -- list modified modules\n"), 
    true. 

dbgtc(File) -> 
    Fun = fun({trace,_,call,{M,F,A}}, _) -> 
                  io:format("call: ~w:~w~w~n", [M,F,A]); 
             ({trace,_,return_from,{M,F,A},R}, _) -> 
                  io:format("retn: ~w:~w/~w -> ~w~n", [M,F,A,R]); 
             (A,B) -> 
                  io:format("~w: ~w~n", [A,B]) 
          end, 
    dbg:trace_client(file, File, {Fun, []}). 

dbgon(Module) -> 
    case dbg:tracer() of 
        {ok,_} -> 
            dbg:p(all,call), 
            dbg:tpl(Module, [{'_',[],[{return_trace}]}]), 
            ok; 
        Else -> 
            Else 
    end. 

dbgon(Module, Fun) when is_atom(Fun) -> 
    {ok,_} = dbg:tracer(), 
    dbg:p(all,call), 
    dbg:tpl(Module, Fun, [{'_',[],[{return_trace}]}]), 
    ok; 

dbgon(Module, File) when is_list(File) -> 
    {ok,_} = dbg:tracer(file, dbg:trace_port(file, File)), 
    dbg:p(all,call), 
    dbg:tpl(Module, [{'_',[],[{return_trace}]}]), 
    ok. 

dbgadd(Module) -> 
    dbg:tpl(Module, [{'_',[],[{return_trace}]}]), 
    ok. 

dbgadd(Module, Fun) -> 
    dbg:tpl(Module, Fun, [{'_',[],[{return_trace}]}]), 
    ok. 

dbgdel(Module) -> 
    dbg:ctpl(Module), 
    ok. 

dbgdel(Module, Fun) -> 
    dbg:ctpl(Module, Fun), 
    ok. 

dbgoff() -> 
    dbg:stop(). 


lm() ->
    [c:l(M) || M <- mm()].

mm() ->
    modified_modules().

modified_modules() ->
    [M || {M, _} <- code:all_loaded(), module_modified(M) == true].

module_modified(Module) ->
    case code:is_loaded(Module) of
        {file, preloaded} ->
            false;
        {file, Path} ->
            CompileOpts = proplists:get_value(compile, Module:module_info()),
            CompileTime = proplists:get_value(time, CompileOpts),
            Src = proplists:get_value(source, CompileOpts),
            module_modified(Path, CompileTime, Src);
        _ ->
            false
    end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
    case find_module_file(Path) of
        false ->
            false;
        ModPath ->
            case beam_lib:chunks(ModPath, ["CInf"]) of
                {ok, {_, [{_, CB}]}} ->
                    CompileOpts = binary_to_term(CB),
                    CompileTime = proplists:get_value(time, CompileOpts),
                    Src = proplists:get_value(source, CompileOpts),
                    not (CompileTime == PrevCompileTime) and (Src == PrevSrc);
                _ ->
                    false
            end
    end.

find_module_file(Path) ->
    case file:read_file_info(Path) of
        {ok, _} ->
            Path;
        _ ->
            %% may be the path was changed?
            case code:where_is_file(filename:basename(Path)) of
                non_existing ->
                    false;
                NewPath ->
                    NewPath
            end
    end.


time(F) when is_function(F) ->
    S = now(), Res = F(), E = now(), {timer:now_diff(E,S), Res}.
