%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @private
%%%-------------------------------------------------------------------
-module(power_shell_default_SUITE).
-author("maximfca@gmail.com").

%%--------------------------------------------------------------------

-export([all/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2]).

-export([inject/0, inject/1,
    already/0, already/1,
    shell_default_inject/0, shell_default_inject/1,
    user_default/0,user_default/1,
    user_default_inject/0, user_default_inject/1,
    failed_inject/0, failed_inject/1]).

%% Common Test headers
-include_lib("common_test/include/ct.hrl").

%% Include stdlib header to enable ?assert() for readable output
-include_lib("stdlib/include/assert.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

all() ->
    [inject, already, shell_default_inject, user_default, user_default_inject, failed_inject].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(TC, Config)  when TC =:= shell_default_inject; TC =:= user_default;
                                    TC =:= user_default_inject; TC =:= failed_inject ->
    Filename = filename:join(proplists:get_value(priv_dir, Config), "user_default"),
    [{user_default, Filename} | Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(TC, Config) when TC =:= shell_default_inject; TC =:= user_default;
                                  TC =:= user_default_inject; TC =:= failed_inject ->
    application:stop(power_shell),
    Filename = proplists:get_value(user_default, Config),
    file:delete(Filename),
    proplists:delete(user_default, Config);
end_per_testcase(_, Config) ->
    Config.

inject() ->
    [{doc, "Tests power_shell injection"}].

inject(_Config) ->
    {'EXIT', {undef, _}} = (catch shell_default:eval(lists, seq, [1,2])),
    ok = power_shell_default:inject(shell_default, power_shell),
    % test injected:
    [1, 2] = shell_default:eval(lists, seq, [1,2]),
    %
    ok = power_shell_default:eject(shell_default, power_shell),
    ok.

already() ->
    [{doc, "Test proxy injection on already augmented code"}].

already(_Config) ->
    {'EXIT', {undef, _}} = (catch shell_default:eval(lists, seq, [1,2])),
    ok = power_shell_default:inject(shell_default, power_shell),
    {error, already_loaded} = power_shell_default:inject(shell_default, power_shell),
    ok = power_shell_default:eject(shell_default, power_shell),
    {error, not_loaded} = power_shell_default:eject(shell_default, power_shell),
    ok.

shell_default_inject() ->
    [{doc, "Test shell_default integration with injected proxy"}].

shell_default_inject(_Config) ->
    ok = application:set_env(power_shell, shell_integration, shell_default),
    ok = application:start(power_shell),
    % so shell_default must be working now, right?
    [1, 2] = shell_default:eval(lists, seq, [1,2]),
    ok.

user_default() ->
    [{doc, "Test user_default integration with no user-defined user_default"}].

user_default(_Config) ->
    ok = application:set_env(power_shell, shell_integration, user_default),
    ok = application:start(power_shell),
    % so shell_default must be working now, right?
    [1, 2] = user_default:eval(lists, seq, [1,2]),
    ok.

user_default_inject() ->
    [{doc, "Test user_default integration with injected proxy"}].

user_default_inject(Config) ->
    make_user_default(Config, user_default, [debug_info]),
    %
    ok = application:set_env(power_shell, shell_integration, user_default),
    ok = application:start(power_shell),
    % ensure 'old' user_default is still there
    true = user_default:exist(),
    % so shell_default must be working now, right?
    [1, 2] = user_default:eval(lists, seq, [1,2]),
    ok.

failed_inject() ->
    [{doc, "Test that failed injection does not crash everything"}].

failed_inject(Config) ->
    % current user_default is broken
    make_user_default(Config, user_default, [debug_info]),
    {module, user_default} = code:ensure_loaded(user_default),
    % break it now
    write_user_default(Config, no_default, []),
    ?assertMatch({error, {bad_return, {{power_shell_app,start,[normal,[]]},
        {'EXIT', {{badmatch,{error,{badmatch,_}}}, _}}}}},
        application:start(power_shell)),
    %
    %
    Beamname = make_user_default(Config, user_default, []),
    % no debug info in the current user_default
    ok = application:set_env(power_shell, shell_integration, user_default),
    ?assertMatch({error, {bad_return, {{power_shell_app,start,[normal,[]]},
        {'EXIT', {{badmatch,{error,no_abstract_code}}, _}}}}},
        application:start(power_shell)),

    % beam file broken
    ok = file:write_file(Beamname, <<"FOR1 broken non-beam file">>),
    ?assertMatch({error, {bad_return, {{power_shell_app,start,[normal,[]]},
        {'EXIT', {{badmatch,{error,{beam_lib, {error,beam_lib,
            {not_a_beam_file, _}}}}}, _}}}}},
        application:start(power_shell)),
    code:purge(user_default),
    code:delete(user_default),
    ok.

make_user_default(Config, Mod, Options) ->
    Filename = write_user_default(Config, Mod, Options),
    % load module from binary
    {module, Mod} = code:load_abs(Filename),
    code:purge(Mod),
    % ensure it was success
    true = Mod:exist(),
    Filename ++ ".beam".

write_user_default(Config, Mod, Options) ->
    Filename = proplists:get_value(user_default, Config),
    % make and load fake user_default module, that has a single function
    Scans = lists:map(fun (Line) ->
        {ok, Scan, _} = erl_scan:string(Line),
        {ok, Form} = erl_parse:parse_form(Scan),
        Form end, ["-module(" ++ atom_to_list(Mod) ++ ").","-export([exist/0]).","exist() -> true."]),
    % compile forms to binary
    {ok, Mod, Bin} = compile:forms(Scans, Options),
    ok = file:write_file(Filename ++ ".beam", Bin),
    Filename.
