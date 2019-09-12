%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @doc
%%% Implements Erlang interpreter via eval/3 function.
%%%
%%% Allows to 'call' functions that are not exported by interpreting
%%% function code.
%%% @end
%%%-------------------------------------------------------------------
-module(power_shell).
-author("maximfca@gmail.com").

%% API
-export([
    eval/3,
    eval/4
]).

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
-spec eval( Module :: module(), Fun :: atom(), Args :: [term()], power_shell_cache:function_map()) ->
    term().
eval(Mod, Fun, Args, FunMap) ->
    eval_apply(erlang:is_builtin(Mod, Fun, length(Args)), Mod, Fun, Args, FunMap).

%%--------------------------------------------------------------------
%% Compatibility: stacktrace

-ifdef(OTP_RELEASE).
    -define(WithStack(Cls, Err, Stk), Cls:Err:Stk).
    -define(GetStack(Stk), Stk).
-else.
    -define(WithStack(Cls, Err, Stk), Cls:Err).
    -define(GetStack(Stk), erlang:get_stacktrace()).
-endif.


%%--------------------------------------------------------------------
%% Internal functions

-define (STACK_TOKEN, '$power_shell_stack_trace').

eval_apply(true, Mod, Fun, Args, _FunMap) ->
    erlang:apply(Mod, Fun, Args);
eval_apply(false, Mod, Fun, Args, FunMap0) ->
    put(?STACK_TOKEN, []),
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
        erase(?STACK_TOKEN)
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
        ?WithStack(error, undef, Stack) ->
            pop_stack(),
            push_stack({M, F, A, []}),
            erlang:raise(error, undef, ?GetStack(Stack))
    end,
    pop_stack(),
    Ret;
do_apply(Fun, A) ->
    push_stack({undefined, Fun, length(A), []}),
    Ret = erlang:apply(Fun, A),
    pop_stack(),
    Ret.
