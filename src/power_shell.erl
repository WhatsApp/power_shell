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
-export([eval/3]).

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
    try eval(Mod, Fun, Args, power_shell_cache:get_module(Mod)) of
        Val ->
            Val
    catch
        error:preloaded ->
            erlang:apply(Mod, Fun, Args);
        error:enoent ->
            erlang:raise(error, undef, [{Mod, Fun, Args, []}]);
        error:{unwind, Class, Reason, Stack} ->
            % TODO: replace file atoms with actual file names in Stack
            erlang:raise(Class, Reason, lists:reverse(Stack))
    end.

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

eval(Mod, Fun, Args, FunMap) ->
    Arity = length(Args),
    case maps:get({Fun, Arity}, FunMap, undefined) of
        undefined ->
            erlang:raise(error, {unwind, error, undef, [{Mod, Fun, Args, []}]}, []);
        {function, _, Fun, Arity, Clauses} ->
            case power_shell_eval:match_clause(Clauses, Args, power_shell_eval:new_bindings(), local_fun_handler(Mod, FunMap)) of
                {Body, Bindings} ->
                    % find line number by reverse scan of clauses, in {clause,_,H,G,B}
                    %%% UGLY %%% - See TODO for eval_exprs
                    {clause, Line, _, _, Body} = lists:keyfind(Body, 5, Clauses),
                    %NewLocalStack = [{Mod, Fun, Arity, [{line, Line}]}],
                    try
                        % power_shell_eval:exprs() does not allow to get the value of the last expr only
                        % power_shell_eval:exprs(Body, Bindings, local_fun_handler(Mod, FunMap)),
                        eval_exprs(Body, Bindings,
                            local_fun_handler(Mod, FunMap),
                            non_local_fun_handler())
                    catch
                        error:{unwind, Class, Reason, Stack} ->
                            erlang:raise(error, {unwind, Class, Reason, [{Mod, Fun, Arity, [{file, Mod}, {line, Line}]} | Stack]}, []);
                        Class:Reason ->
                            % remove stack items common with current stack
                            erlang:raise(error, {unwind, Class, Reason, [{Mod, Fun, Arity, [{file, Mod}, {line, Line}]}]}, [])
                    end;
                nomatch ->
                    erlang:raise(error, {unwind, error, function_clause, [{Mod, Fun, Args, []}]}, [])
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
                eval(Mod, Fun, Args, FunMap)
        end
    }.

%% Non-local handler serves following purposes:
%   * catch exception thrown and fixup stack
non_local_fun_handler() ->
    {
        value,
        fun (MF, A) ->
            try do_apply(MF, A) of
                Val ->
                    Val
            catch
                ?WithStack(error, undef, Stack) ->
                    erlang:raise(error, {unwind, error, undef, [hd(?GetStack(Stack))]}, []);
                Class:Reason ->
                    erlang:raise(error, {unwind, Class, Reason, []}, [])
            end
        end
    }.

do_apply({M, F}, A) ->
    erlang:apply(M, F, A);
do_apply(Fun, A) ->
    erlang:apply(Fun, A).
