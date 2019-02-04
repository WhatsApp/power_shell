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
    remote_callback_exported/0, remote_callback_exported/1]).

-export([export_all/0, remote_cb_exported/1]).

-include_lib("common_test/include/ct.hrl").

% eunit: convenient asserts, really!
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

test_cases() ->
    [echo, preloaded, second_clause, undef, undef_local, undef_nested, recursive,
        calling_local, throwing, bad_match, function_clause, remote_callback,
        callback_local, callback_local_fun_obj, callback_local_make_fun,
        remote_callback_exported].
    %[remote_callback].
    %[remote_callback].

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
    [{direct,
        test_cases()
     }, {cached,
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
    application:ensure_started(power_shell),
    ok = application:set_env(power_shell, cache_code, true),
    ok = application:stop(power_shell),
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
    ok = application:start(power_shell),
    ok;
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% IMPORTANT: THESE MUST NOT BE EXPORTED !!!

local_unexported(Echo) ->
    Echo.

local_unexported_recursive(N, Acc) when N =< 0 ->
    Acc;
local_unexported_recursive(N, Acc) ->
    local_unexported_recursive(N - 1, [N | Acc]).

local_unexported_nested(N) ->
    local_unexported_recursive(N, []).

local_undef_nested() ->
    not_a_module:not_a_function(1, one, 3),
    ok. % to avoid tail recursion

local_second_clause(Arg, Selector) when Selector =:= one ->
    Arg + 1;
local_second_clause(Arg, Selector) when Selector =:= two ->
    Arg + 2;
local_second_clause(Arg, Selector) when Selector =:= three ->
    Arg + 3;
local_second_clause(Arg, Selector) when is_atom(Selector) ->
    Arg + 10.

local_throw(What) ->
    throw(What).

local_throwing() ->
    local_throw(ball),
    % to make it non-tail-recursive and save call stack:
    ok.

local_bad_match() ->
    local_do_bad_match(one),
    ok.

local_do_bad_match(What) ->
    two = What,
    ok.

local_function_clause() ->
    local_second_clause(0, 0),
    ok.

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
    local_undef_nested(),
    local_cb_fun(1),
    local_throwing(),
    local_bad_match().

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
echo() ->
    [{doc}, "Evaluate non-exported function"].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
echo(_Config) ->
    ?assertEqual(local_unexported(echo), power_shell:eval(?MODULE, local_unexported, [echo])),
    ok.

undef() ->
    [{doc, "Ensure undefined function throws undef"}].

undef(_Config) ->
    ?assertEqual({'EXIT', {undef, [{not_a_module,not_a_function,[1,2,3], []}]}},
        (catch power_shell:eval(not_a_module, not_a_function, [1, 2, 3]))),
    ok.

undef_local() ->
    [{doc, "Ensure undefined function in this very module throws undef"}].

undef_local(_Config) ->
    ?assertEqual({'EXIT', {undef, [{?MODULE,not_a_function,[1,2,3], []}]}},
        (catch power_shell:eval(?MODULE, not_a_function, [1, 2, 3]))),
    ok.

undef_nested() ->
    [{doc, "Ensure undefined function throws undef even when it's nested"}].

undef_nested(_Config) ->
    exception_check(fun local_undef_nested/0, local_undef_nested).

preloaded() ->
    [{doc, "Ensure that functions from preloaded modules are just applied"}].

preloaded(_Config) ->
    ?assertEqual(self(), power_shell:eval(erlang, self, [])).

throwing() ->
    [{doc, "Unexported function throwing"}].

throwing(_Config) ->
    exception_check(fun local_throwing/0, local_throwing).

bad_match() ->
    [{doc, "Unexported function throwing badmatch"}].

bad_match(_Config) ->
    exception_check(fun local_bad_match/0, local_bad_match).

function_clause() ->
    [{doc, "Unexported function throwing function_clause"}].

function_clause(_Config) ->
    exception_check(fun local_function_clause/0, local_function_clause).

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

%%--------------------------------------------------------------------
%% Excepton testing helper
%%--------------------------------------------------------------------
%% Compatibility: stacktrace

-ifdef(OTP_RELEASE).
    -define(WithStack(Cls, Err, Stk), Cls:Err:Stk).
    -define(GetStack(Stk), Stk).
-else.
    -define(WithStack(Cls, Err, Stk), Cls:Err).
    -define(GetStack(Stk), erlang:get_stacktrace()).
-endif.

exception_check(Fun, FunAtomName) ->
    Expected = try Fun() of
                   Val ->
                       throw({must_not_return, test_broken, Val})
               catch
                   ?WithStack(E0, X0, Stack0) ->
                       % exclude own stack
                       Own = tl(element(2, erlang:process_info(self(), current_stacktrace))),
                       {E0, X0, [{M, F, A, []} || {M, F, A, _} <- lists:droplast(?GetStack(Stack0) -- Own)]}
               end,
    Actual = try power_shell:eval(?MODULE, FunAtomName, []) of
                 Val1 ->
                     Val1
             catch
                 ?WithStack(E, X, Stack1) ->
                     {E, X, [{M, F, A, []} || {M, F, A, _} <- ?GetStack(Stack1)]}
             end,
    % allow line numbers and file names to slip through
    ?assertEqual(Expected, Actual).
