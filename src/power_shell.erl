%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @doc
%%% Implements Erlang interpreter via eval/3 function.
%%%
%%% Allows to `call' functions that are not exported by interpreting
%%% function code.
%%%
%%% For cases when evaluation is too slow, it's possible to recompile
%%% a module and hot-code-load it using `export/1,2,3', and later
%%% revert the change with `revert/1'.
%%% @end
%%%-------------------------------------------------------------------
-module(power_shell).
-author("maximfca@gmail.com").

%% API
-export([
    eval/3,
    eval/4,
    export/1,
    export/2,
    export/3,
    revert/1
]).

%% Internal exports
-export([sentinel/3]).

%%--------------------------------------------------------------------
%% API

%% @doc Performs erlang:apply(Module, Fun, Args) by evaluating AST
%%      of Module:Fun.
%% @param Module Module name, must be either loaded or discoverable with code:which() or filelib:find_source()
%% @param Fun function name, may not be exported
%% @param Args List of arguments
-spec eval( Module :: module(), Fun :: atom(), Args :: [term()]) ->
    term().

eval(Mod, Fun, Args) when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    eval_apply(erlang:is_builtin(Mod, Fun, length(Args)), Mod, Fun, Args, undefined).

%% @doc Performs erlang:apply(Module, Fun, Args) by evaluating AST
%%      of Module:Fun.
%% @param Module Module name, must be either loaded or discoverable with code:which() or filelib:find_source()
%% @param Fun function name, may not be exported
%% @param Args List of arguments
%% @param FunMap AST of all functions defined in Mod, as returned by power_shell_cache:get_module(Mod)
%%        Should be used if starting power_shell_cache gen_server is undesirable.
-spec eval(Module :: module(), Fun :: atom(), Args :: [term()], power_shell_cache:function_map()) ->
    term().
eval(Mod, Fun, Args, FunMap) ->
    eval_apply(erlang:is_builtin(Mod, Fun, length(Args)), Mod, Fun, Args, FunMap).

%% @equiv export(Module, all, #{})
-spec export(Module :: module()) -> pid().
export(Mod) ->
    export(Mod, all).

%% @equiv export(Module, Export, #{})
-spec export(Module :: module(), Export :: all | [{Fun :: atom(), Arity :: non_neg_integer()}]) -> pid().
export(Mod, Export) ->
    export(Mod, Export, #{}).

-type export_options() :: #{
    link => boolean()
}.

%% @doc Retrieves code (AST) of the `Module' from the debug information chunk. Exports all or selected
%%      functions and reloads the module.
%%      A sentinel process is created for every `export' call, linked to the caller process. When
%%      the calling process terminates, linked sentinel loads the original module back and
%%      terminates. Sentinel process can be stopped gracefully by calling `revert(Sentinel)'.
%%      There is no protection against multiple `export' calls interacting, and caller is
%%      responsible for proper synchronisation. Use `eval' when no safe sequence can be found.
%% @param Module Module name, must be either loaded or discoverable with code:which()
%% @param Export list of tuples `{Fun, Arity}' to be exported, or `all' to export all functions.
%% @param Options use `#{link => false}' to start the sentinel process unlinked, assuming manual
%%       `revert' call.
-spec export(Module :: module(), all | [{Fun :: atom(), Arity :: non_neg_integer()}],
        export_options()) -> pid().
export(Mod, Export, #{link := false}) ->
    proc_lib:start(?MODULE, sentinel, [self(), Mod, Export]);
export(Mod, Export, _Options) ->
    proc_lib:start_link(?MODULE, sentinel, [self(), Mod, Export]).

%% @doc Gracefully stops the sentinel process, causing the original module to be loaded back.
%% @param Sentinel process to stop.
-spec revert(Sentinel :: pid()) -> ok.
revert(Sentinel) ->
    proc_lib:stop(Sentinel).

%%--------------------------------------------------------------------
%% Internal functions
%% Sentinel process
sentinel(Parent, Mod, Export) ->
    Options = proplists:get_value(options, Mod:module_info(compile), []),
    {Mod, Binary, File}  = code:get_object_code(Mod),
    {ok, {Mod, [{abstract_code, {_, Forms}}]}} = beam_lib:chunks(Binary, [abstract_code]),
    Expanded = erl_expand_records:module(Forms, [strict_record_tests]),
    {ok, Mod, Bin} = make_export(Expanded, Options, Export),
    %% trap exits before loading the code
    erlang:process_flag(trap_exit, true),
    {module, Mod} = code:load_binary(Mod, File, Bin),
    proc_lib:init_ack(Parent, self()),
    receive
        {'EXIT', Parent, _Reason} ->
            {module, Mod} = code:load_binary(Mod, File, Binary);
        {system, Reply, {terminate, _Reason}} ->
            {module, Mod} = code:load_binary(Mod, File, Binary),
            gen:reply(Reply, ok)
    end.

make_export(Forms, Options, all) ->
    compile:forms(Forms, [export_all, binary, debug_info] ++ Options);
make_export(Forms, Options, Exports) ->
    Exported = insert_export(Forms, [], Exports),
    compile:forms(Exported, [binary, debug_info] ++ Options).

%% don't handle modules that export nothing, crash instead, for these modules
%%  aren't useful anyway.
insert_export([{attribute, Anno, export, _} = First | Tail], Passed, Exports) ->
    ExAnno = erl_anno:new(erl_anno:line(Anno)),
    lists:reverse(Passed) ++ [First, {attribute, ExAnno, export, Exports} | Tail];
insert_export([Form | Tail], Passed, Exports) ->
    insert_export(Tail, [Form | Passed], Exports).

%%--------------------------------------------------------------------
%% Evaluator

-define (STACK_TOKEN, '$power_shell_stack_trace').

eval_apply(true, Mod, Fun, Args, _FunMap) ->
    erlang:apply(Mod, Fun, Args);
eval_apply(false, Mod, Fun, Args, FunMap0) ->
    PreservedStack = put(?STACK_TOKEN, []),
    try
        FunMap = if FunMap0 =:= undefined -> power_shell_cache:get_module(Mod); true -> FunMap0 end,
        eval_impl(Mod, Fun, Args, FunMap)
    catch
        error:enoent ->
            {current_stacktrace, Trace} = process_info(self(), current_stacktrace),
            erlang:raise(error, undef, [{Mod, Fun, Args, []}] ++ tl(Trace));
        throw:Reason ->
            {current_stacktrace, Trace} = process_info(self(), current_stacktrace),
            % recover stack from process dictionary
            pop_stack(),
            erlang:raise(throw, Reason, get_stack() ++ tl(Trace));
        Class:Reason ->
            {current_stacktrace, Trace} = process_info(self(), current_stacktrace),
            % recover stack from process dictionary
            erlang:raise(Class, Reason, get_stack() ++ tl(Trace))
    after
        case PreservedStack of
            undefined -> erase(?STACK_TOKEN);
            _ when is_list(PreservedStack) -> put(?STACK_TOKEN, PreservedStack)
        end
    end.

push_stack(MFA) ->
    put(?STACK_TOKEN, [MFA | get(?STACK_TOKEN)]).

pop_stack() ->
    put(?STACK_TOKEN, tl(get(?STACK_TOKEN))).

get_stack() ->
    get(?STACK_TOKEN).
    %[{M, F, length(Args), Dbg} || {M, F, Args, Dbg} <- Stack].
    %[hd(Stack) | [{M, F, length(Args)} || {M, F, Args} <- tl(Stack)]].

eval_impl(Mod, Fun, Args, FunMap) ->
    Arity = length(Args),
    case maps:get({Fun, Arity}, FunMap, undefined) of
        undefined ->
            push_stack({Mod, Fun, Args, []}),
            erlang:raise(error, undef, [{Mod, Fun, Args, []}]);
        {function, _, Fun, Arity, Clauses} ->
            case power_shell_eval:match_clause(Clauses, Args, power_shell_eval:new_bindings(), local_fun_handler(Mod, FunMap)) of
                {Body, Bindings} ->
                    % find line number by reverse scan of clauses, in {clause,_,H,G,B}
                    %%% UGLY %%% - See TODO for eval_exprs
                    {clause, _Line, _, _, Body} = lists:keyfind(Body, 5, Clauses),
                    %NewLocalStack = [{Mod, Fun, Arity, [{line, Line}]}],
                    push_stack({Mod, Fun, length(Args), []}),
                    % power_shell_eval:exprs() does not allow to get the value of the last expr only
                    % power_shell_eval:exprs(Body, Bindings, local_fun_handler(Mod, FunMap)),
                    R = eval_exprs(Body, Bindings,
                        local_fun_handler(Mod, FunMap),
                        non_local_fun_handler()),
                    pop_stack(),
                    R;
                nomatch ->
                    push_stack({Mod, Fun, Args, []}),
                    erlang:raise(error, function_clause, [{Mod, Fun, Args, []}])
            end
    end.

%% TODO: find a way to supply Line Number (every Expr has it) for Stack purposes
eval_exprs([Expr], Bindings, LocalFun, NonLocalFun) ->
    power_shell_eval:expr(Expr, Bindings, LocalFun, NonLocalFun, value);
eval_exprs([Expr | Exprs], Bindings, LocalFun, NonLocalFun) ->
    {value, _Value, NewBindings} = power_shell_eval:expr(Expr, Bindings, LocalFun, NonLocalFun),
    eval_exprs(Exprs, NewBindings, LocalFun, NonLocalFun).

local_fun_handler(Mod, FunMap) ->
    {
        value,
        fun ({function, Fun, Arity}, []) ->
                % extract body for this locally-defined function
                % requires patched erl_eval, thus a need for power_shell_eval.
                % we rely on the fact that AST has been compiled without errors,
                %   which means local functions are surely defined
                FunArity = {Fun, Arity},
                #{FunArity := {function, _, Fun, Arity, Clauses}} = FunMap,
                Clauses;
            (Fun, Args) ->
                eval_impl(Mod, Fun, Args, FunMap)
        end
    }.

%% Non-local handler serves following purposes:
%   * catch exception thrown and fixup stack
non_local_fun_handler() ->
    {
        value, fun do_apply/2
    }.

do_apply({M, F}, A) ->
    push_stack({M, F, length(A), []}),
    Ret = try
        erlang:apply(M, F, A)
    catch
        error:undef:Stack ->
            pop_stack(),
            push_stack({M, F, A, []}),
            erlang:raise(error, undef, Stack)
    end,
    pop_stack(),
    Ret;
do_apply(Fun, A) ->
    [{Mod, Name, _, _} | _] = get_stack(), % assume that anonymous fun comes from the same module & function
    Lambda = list_to_atom(atom_to_list(Name) ++ "-fun-$"),
    push_stack({Mod, Lambda, length(A), []}),
    Ret = erlang:apply(Fun, A),
    pop_stack(),
    Ret.
