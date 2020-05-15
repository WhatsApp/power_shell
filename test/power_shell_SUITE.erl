%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @private
%%%-------------------------------------------------------------------
-module(power_shell_SUITE).
-author("maximfca@gmail.com").

%%--------------------------------------------------------------------
%% IMPORTANT: DO NOT USE EXPORT_ALL!
%% This test relies on functions _not_ being exported from the module.
%% It is the whole point of test.

-export([all/0,
    groups/0,
    init_per_group/2,
    end_per_group/2,
    suite/0]).

-export([echo/0, echo/1,
    self_echo/0, self_echo/1,
    calling_local/0, calling_local/1,
    second_clause/0, second_clause/1,
    undef/0, undef/1,
    undef_local/0, undef_local/1,
    recursive/0, recursive/1,
    undef_nested/0, undef_nested/1,
    bad_match/0, bad_match/1,
    function_clause/0, function_clause/1,
    preloaded/0, preloaded/1,
    throwing/0, throwing/1,
    callback_local/0, callback_local/1,
    callback_local_fun_obj/0, callback_local_fun_obj/1,
    remote_callback/0, remote_callback/1,
    callback_local_make_fun/0, callback_local_make_fun/1,
    remote_callback_exported/0, remote_callback_exported/1,
    record/0, record/1,
    try_side_effect/0, try_side_effect/1,
    rebind_var/0, rebind_var/1,
    external_fun/0, external_fun/1,
    catch_apply/0, catch_apply/1
]).

-export([export_all/0, remote_cb_exported/1]).

%% Common Test headers
-include_lib("common_test/include/ct.hrl").

%% Include stdlib header to enable ?assert() for readable output
-include_lib("stdlib/include/assert.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

test_cases() ->
    [echo, self_echo, preloaded, second_clause, undef, undef_local, undef_nested, recursive,
        calling_local, throwing, bad_match, function_clause, remote_callback,
        callback_local, callback_local_fun_obj, callback_local_make_fun,
        remote_callback_exported, record, try_side_effect, rebind_var, external_fun, catch_apply].

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [{direct, [parallel],
        test_cases()
     }, {cached, [parallel],
         test_cases()
     }].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [{group, direct}, {group, cached}].

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(cached, Config) ->
    % ensure power_shell cache started
    Loaded = application:load(power_shell),
    ?assert(Loaded =:= ok orelse Loaded =:= {error,{already_loaded,power_shell}}),
    ok = application:set_env(power_shell, cache_code, true),
    ok = application:start(power_shell),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(cached, _Config) ->
    ok = application:unset_env(power_shell, cache_code),
    ok = application:stop(power_shell),
    ok;
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% IMPORTANT: THESE MUST NOT BE EXPORTED !!!

local_unexported(Echo) ->
    Echo.

local_self() ->
    erlang:self().

local_unexported_recursive(N, Acc) when N =< 0 ->
    Acc;
local_unexported_recursive(N, Acc) ->
    local_unexported_recursive(N - 1, [N | Acc]).

local_unexported_nested(N) ->
    local_unexported_recursive(N, []).

local_undef_nested(Atom) ->
    local_undef_nested_impl(Atom),
    true = rand:uniform(100). % to avoid tail recursion

local_undef_nested_impl(Atom) ->
    not_a_module:not_a_function(1, Atom, 3),
    true = rand:uniform(100). % to avoid tail recursion

local_second_clause(Arg, Selector) when Selector =:= one ->
    Arg + 1;
local_second_clause(Arg, Selector) when Selector =:= two ->
    Arg + 2;
local_second_clause(Arg, Selector) when Selector =:= three ->
    Arg + 3;
local_second_clause(Arg, Selector) when is_atom(Selector) ->
    Arg + 10.

local_throw(What) ->
    rand:uniform(100) < 200 andalso throw(What).

local_throwing() ->
    local_throw(ball),
    % to make it non-tail-recursive and save call stack:
    ok.

local_bad_match() ->
    local_do_bad_match(one),
    true = rand:uniform(100).

local_do_bad_match(What) ->
    true = What =:= rand:uniform(100),
    ok.

local_function_clause() ->
    local_second_clause(0, get(test_server_logopts)),
    true = rand:uniform(100).

local_cb_fun(Arg) ->
    {cb, Arg}.

local_cb_caller(Fun, Args) ->
    Fun(Args).

local_cb_init() ->
    local_cb_caller(fun local_cb_fun/1, [1]).

local_cb_fun_obj() ->
    local_cb_caller(fun ([N]) -> N * 2 end, [2]).

local_cb_make_fun() ->
    F = erlang:make_fun(?MODULE, module_info, 1),
    local_cb_caller(F, md5).

remote_cb(N) when N <0 ->
    N rem 3 =:= 0;
remote_cb(N) ->
    N rem 2 =:= 0.

remote_cb_init(List) ->
    Cb = fun remote_cb/1,
    case List of
        [] ->
            ok;
        [E] ->
            Cb(E);
        [_ | _] ->
            lists:filter(fun remote_cb/1, List)
    end,
    %lists:filter(fun (K) -> K + 1, F = fun remote_cb/1, F(K) end, List).
    lists:filter(Cb, List).

remote_cb_exported(N) ->
    N rem 3 =:= 0.

remote_cb_exported_init(List) ->
    lists:filter(fun ?MODULE:remote_cb_exported/1, List).

%% Kitchen sink to silence compiler in a good way (without suppressing warnings)
export_all() ->
    local_undef_nested(atom),
    local_cb_fun(1),
    local_throwing(),
    local_bad_match(),
    rebind([]),
    external_filter([]),
    throw_applied(),
    try_side({1, 1}).

-record(rec, {first= "1", second, third = initial}).
create_record() ->
    #rec{third = application:get_env(kernel, missing, 3), first = "1"}.

modify_record(#rec{} = Rec) ->
    Rec#rec{third = 10, second = "2"}.

try_side(MaybeBinaryInteger) ->
    try
        _ = binary_to_integer(MaybeBinaryInteger),
        true
    catch
        error:badarg ->
            false
    end.

internal_lambda(Atom) ->
    RebindVar = atom_to_list(Atom),
    is_list(RebindVar).

rebind(Original) ->
    RebindVar = Original,
    lists:filtermap(fun internal_lambda/1, RebindVar).

external_filter(List) ->
    lists:filter(fun erlang:is_number/1, List).

throw_applied() ->
    Args = [[fun() -> exit(some_error) end, []], []], % this generates: {'EXIT',{{badfun,[#Fun<power_shell_SUITE.21.126501267>,[]]},
    case catch apply(erlang, apply, Args) of
        {'EXIT', _Reason} ->
            throw(expected)
    end.

%%--------------------------------------------------------------------
%% Test Cases

echo() ->
    [{doc}, "Evaluate non-exported function"].

echo(_Config) ->
    ?assertEqual(local_unexported(echo), power_shell:eval(?MODULE, local_unexported, [echo])),
    ok.

self_echo() ->
    [{doc}, "Evaluates function returning self() - to see if NIFs are working"].

self_echo(_Config) ->
    ?assertEqual(local_self(), power_shell:eval(?MODULE, local_self, [])),
    ok.

undef() ->
    [{doc, "Ensure undefined function throws undef"}].

undef(_Config) ->
    % next 2 statements must be on the same line, otherwise stack trace info is broken
    {current_stacktrace, Trace} = process_info(self(), current_stacktrace), Actual = (catch power_shell:eval(not_a_module, not_a_function, [1, 2, 3])),
    Expected = {'EXIT', {undef, [{not_a_module, not_a_function,[1,2,3], []}] ++ Trace}},
    ?assertEqual(Expected, Actual),
    ok.

undef_local() ->
    [{doc, "Ensure undefined function in this very module throws undef"}].

undef_local(_Config) ->
    % next 2 statments must be on the same line, otherwise stack trace info is broken
    {current_stacktrace, Trace} = process_info(self(), current_stacktrace), Actual = (catch power_shell:eval(?MODULE, not_a_function, [1, 2, 3])),
    Expected = {'EXIT', {undef, [{?MODULE,not_a_function,[1,2,3], []}] ++ Trace}},
    ?assertEqual(Expected, Actual),
    ok.

undef_nested() ->
    [{doc, "Ensure undefined function throws undef even when it's nested"}].

undef_nested(_Config) ->
    exception_check(fun local_undef_nested/1, local_undef_nested, [atom]).

preloaded() ->
    [{doc, "Ensure that functions from preloaded modules are just applied"}].

preloaded(_Config) ->
    ?assertEqual([2, 1], power_shell:eval(prim_zip, reverse, [[1, 2]])),
    ?assertEqual(self(), power_shell:eval(erlang, self, [])).

throwing() ->
    [{doc, "Unexported function throwing"}].

throwing(_Config) ->
    exception_check(fun local_throwing/0, local_throwing, []).

bad_match() ->
    [{doc, "Unexported function throwing badmatch"}].

bad_match(_Config) ->
    exception_check(fun local_bad_match/0, local_bad_match, []).

function_clause() ->
    [{doc, "Unexported function throwing function_clause"}].

function_clause(_Config) ->
    exception_check(fun local_function_clause/0, local_function_clause, []).

recursive() ->
    [{doc, "Evaluate recursive function"}].

recursive(_Config) ->
    ?assertEqual(local_unexported_recursive(1, []), power_shell:eval(?MODULE, local_unexported_recursive, [1, []])),
    ok.

calling_local() ->
    [{doc, "Evaluate non-exported function calling another non-exported function"}].

calling_local(_Config) ->
    ?assertEqual(local_unexported_nested(0), power_shell:eval(?MODULE, local_unexported_nested, [0])),
    ok.

second_clause() ->
    [{doc, "Evaluate non-exported function with multiple clauses, ensure right one is selected"}].

second_clause(_Config) ->
    ?assertEqual(local_second_clause(10, two), power_shell:eval(?MODULE, local_second_clause, [10, two])),
    ok.

callback_local() ->
    [{doc, "Non-exported function calls some function that calls a callback fun"}].

callback_local(_Config) ->
    ?assertEqual(local_cb_init(), power_shell:eval(?MODULE, local_cb_init, [])),
    ok.

callback_local_fun_obj() ->
    [{doc, "Non-exported function calls some function that calls a callback function object"}].

callback_local_fun_obj(_Config) ->
    ?assertEqual(local_cb_fun_obj(), power_shell:eval(?MODULE, local_cb_fun_obj, [])),
    ok.

callback_local_make_fun() ->
    [{doc, "Non-exported function and make_fun/3 fun"}].

callback_local_make_fun(_Config) ->
    ?assertEqual(local_cb_make_fun(), power_shell:eval(?MODULE, local_cb_make_fun, [])),
    ok.

remote_callback_exported() ->
    [{doc, "Non-exported function calls remote function that calls exported callback"}].

remote_callback_exported(_Config) ->
    L = lists:seq(1, 20),
    ?assertEqual(remote_cb_exported_init(L), power_shell:eval(?MODULE, remote_cb_exported_init, [L])),
    ok.

remote_callback() ->
    [{doc, "Non-exported function calls some function that calls a callback fun"}].

remote_callback(_Config) ->
    L = lists:seq(1, 20),
    ?assertEqual(remote_cb_init(L), power_shell:eval(?MODULE, remote_cb_init, [L])),
    ok.

record() ->
    [{doc, "Tests records - creation & modification"}].

record(_Config) ->
    Rec = create_record(),
    ?assertEqual(Rec, power_shell:eval(?MODULE, create_record, [])),
    ?assertEqual(modify_record(Rec), power_shell:eval(?MODULE, modify_record, [Rec])).

try_side_effect() ->
    [{doc, "Tests try ... catch returning value from both flows"}].

try_side_effect(_Config) ->
    ?assertEqual(false, power_shell:eval(?MODULE, try_side, [atom])).

rebind_var() ->
    [{doc, "Tests that variable is unbound when returned from function"}].

rebind_var(Config) when is_list(Config) ->
    ?assertEqual([atom, atom], power_shell:eval(?MODULE, rebind, [[atom, atom]])).

external_fun() ->
    [{doc, "Tests external function passed to lists:filter"}].

external_fun(Config) when is_list(Config) ->
    ?assertEqual([1, 2], power_shell:eval(?MODULE, external_filter, [[1, atom, 2, atom]])).

catch_apply() ->
    [{doc, "Tests that cast catch erlang:apply works and throws as expected, not converting it to badarg"}].

catch_apply(Config) when is_list(Config) ->
    ?assertThrow(expected, power_shell:eval(?MODULE, throw_applied, [])).

%%--------------------------------------------------------------------
%% Exception testing helper
%%--------------------------------------------------------------------
%% Compatibility: stacktrace

strip_dbg(Trace) ->
    [{M, F, A} || {M, F, A, _Dbg} <- Trace].

-ifdef(OTP_RELEASE).
    -define(WithStack(Cls, Err, Stk), Cls:Err:Stk).
    -define(GetStack(Stk), strip_dbg(Stk)).
-else.
    -define(WithStack(Cls, Err, Stk), Cls:Err).
    -define(GetStack(Stk), strip_dbg(erlang:get_stacktrace())).
-endif.

exception_check(Fun, FunAtomName, Args) ->
    % again, next line cannot be split, otherwise line information would be broken
    Expected = try erlang:apply(Fun, Args) of Val -> throw({test_broken, Val}) catch ?WithStack(C, R, S) -> {C, R, ?GetStack(S)} end, Actual = try power_shell:eval(?MODULE, FunAtomName, Args) of
                 Val1 ->
                     throw({test_broken, Val1})
             catch
                 ?WithStack(Class, Reason, Stack) ->
                     {Class, Reason, ?GetStack(Stack)}
             end,
    % allow line numbers and file names to slip through
    ?assertEqual(Expected, Actual).
