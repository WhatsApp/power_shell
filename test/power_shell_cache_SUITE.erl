%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @private
%%%-------------------------------------------------------------------
-module(power_shell_cache_SUITE).
-author("maximfca@gmail.com").

%%--------------------------------------------------------------------

-export([all/0,
    suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
    ]).

-export([get_module/0, get_module/1,
    bad_calls/0, bad_calls/1,
    start_stop/0, start_stop/1,
    wrong_module/0, wrong_module/1,
    cache_md5_check/0, cache_md5_check/1,
    not_loaded/0, not_loaded/1,
    no_debug_info/0, no_debug_info/1,
    cover_compiled/0, cover_compiled/1,
    cover_compiled_direct/0, cover_compiled_direct/1,
    parse_transform/0, parse_transform/1,
    source_beam_select/0, source_beam_select/1,
    source_reload/0, source_reload/1,
    no_beam/0, no_beam/1,
    on_load/0, on_load/1,
    no_on_load/0, no_on_load/1,
    broken_beam/0, broken_beam/1]).

%% Common Test headers
-include_lib("common_test/include/ct.hrl").

%% Include stdlib header to enable ?assert() for readable output
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").

-define(SERVER, power_shell_cache).
-define(VICTIM, power_shell_SUITE).

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [get_module, bad_calls, start_stop, cache_md5_check, not_loaded, cover_compiled_direct,
        no_debug_info, cover_compiled, parse_transform, source_beam_select,
        source_reload, no_beam, on_load, no_on_load, broken_beam, wrong_module].

init_per_suite(Config) ->
    Loaded = application:load(power_shell),
    ?assert(Loaded =:= ok orelse Loaded =:= {error,{already_loaded,power_shell}}),
    ok = application:set_env(power_shell, cache_code, true),
    {ok, _} = application:ensure_all_started(power_shell),
    % make a backup of power_shell_SUITE.beam
    Path = code:which(?VICTIM),
    % into private data dir
    PrivPath = ?config(priv_dir, Config),
    BackupPath = [PrivPath, filename:basename(Path)],
    {ok, _} = file:copy(Path, BackupPath),
    [{beam_backup, {Path, BackupPath}} | Config].

end_per_suite(Config) ->
    case proplists:get_value(beam_backup, Config, undefined) of
        {Path, BackupPath} ->
            file:copy(BackupPath, Path);
        _ ->
            ok
    end,
    ok = application:unset_env(power_shell, cache_code),
    ok = application:stop(power_shell),
    Config.

init_per_testcase(cover_compiled_direct, Config) ->
    ok = application:stop(power_shell),
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(cover_compiled_direct, Config) ->
    ok = application:start(power_shell),
    Config;
end_per_testcase(_TestCase, Config) ->
    Config.

touch_file(Filename) ->
    {ok, FileInfo} = file:read_file_info(Filename, [{time, posix}]),
    ok = file:write_file_info(Filename,
        FileInfo#file_info{mtime = FileInfo#file_info.mtime + 2}, [{time, posix}]).

get_counter(false, Fun, Arity) ->
    Pattern = {{bump, power_shell_default, Fun, Arity, '_', '$1'}, '$2'},
    Lines = ets:match(cover_internal_data_table, Pattern),
    [_ExpectedLine, Counter] = hd(Lines),
    Counter;
get_counter(true, Fun, Arity) ->
    Pattern = {{bump, power_shell_default, Fun, Arity, '_', '$1'}, '$2'},
    Lines = ets:match(cover_internal_mapping_table, Pattern),
    [_ExpectedLine, CounterIndex] = hd(Lines),
    counters:get(persistent_term:get({cover, power_shell_default}), CounterIndex).

cover_compile_test(TestFun) ->
    application:load(tools),
    {ok, Vsn} = application:get_key(tools, vsn),
    [Major, Minor | _] = [list_to_integer(V) || V <- string:lexemes(Vsn, ".")],
    Modern = Major >= 3 andalso Minor >= 2,
    % power_shell itself should be cover-compiled, ensure it is so
    ?assertEqual(cover_compiled, code:which(power_shell_default)),
    % ensure decompilation works on cover-compiled files
    FunMap = power_shell_cache:get_module(power_shell_default),
    ?assertNotEqual(error, maps:find({proxy_fun, 3}, FunMap)),
    % now verify that cover-compiled modules are actually bumping their counters
    %% new code for OTP 22
    PrevCounter = get_counter(Modern, proxy_fun, 3),
    % "we need to go deeper" - now within 4 apply levels
    Reps = 10,
    % do this "Reps" times
    [
        ?assertMatch({function,1,node,0, _}, TestFun())
        || _ <- lists:seq(1, Reps)],
    % Get the counter again
    NextCounter = get_counter(Modern, proxy_fun, 3),
    ?assertEqual(Reps, NextCounter - PrevCounter).

get_module() ->
    [{doc, "Tests decompilation from debug_info, an check it is cached"}].

get_module(_Config) ->
    code:delete(?VICTIM),
    ?assert(is_map(power_shell_cache:get_module(?VICTIM))),
    {state, #{?VICTIM := ModInfo}} = sys:get_state(power_shell_cache),
    ?assertEqual(module_data, element(1, ModInfo)),
    % ugly check via 'coverage' info
    % does not really verify that cache is used, TODO: implement the check
    ?assert(is_map(power_shell_cache:get_module(?VICTIM))).

bad_calls() ->
    [{doc, "Tests invalid casts and calls to cache server"}].

bad_calls(_Config) ->
    ?assertEqual({error, {undef, bad_calls}},
        gen_server:call(?SERVER, bad_calls)),
    % no report from cast or info, but at least test it does not crash
    % that's mostly for coverage
    gen_server:cast(?SERVER, cast),
    ok.

start_stop() ->
    [{doc, "Tests cache start/stop procedures"}].

start_stop(_Config) ->
    ok = supervisor:terminate_child(power_shell_sup, ?SERVER),
    {ok, _Pid} = supervisor:restart_child(power_shell_sup, ?SERVER).

not_loaded() ->
    [{doc, "Tests interpretation of not yet loaded *.beam"}].

not_loaded(_Config) ->
    code:delete(?VICTIM),
    ?assertNot(code:is_loaded(?VICTIM)),
    %
    ?assertEqual(echo, power_shell:eval(?VICTIM, local_unexported, [echo])),
    %
    ?assertNot(code:is_loaded(?VICTIM)),
    ok.

cache_md5_check() ->
    [{doc, "Test cache reloading BEAM when in-memory md5 changes"}].

cache_md5_check(_Config) ->
    % ensure current version is cached
    code:ensure_loaded(?VICTIM),
    power_shell_cache:get_module(?VICTIM),
    {state, #{?VICTIM := ModInfo}} = sys:get_state(power_shell_cache),
    ?assertEqual(?VICTIM:module_info(md5), element(2, ModInfo)),
    % replace module so its md5 changes
    Filename = code:which(?VICTIM),
    {ok, ?VICTIM, Chunks} = beam_lib:all_chunks(Filename),
    % add atom 'none' to the tail of the atom list
    {value, {"AtU8", <<Count:32, AtomChunk/binary>>}, AllButAtoms} =
        lists:keytake("AtU8", 1, Chunks),
    Count1 = Count + 1,
    NewAtomChunk = <<Count1:32, AtomChunk/binary, 4:32, <<"none">>/binary>>,
    {ok, Bin} = beam_lib:build_module([{"AtU8", NewAtomChunk} | AllButAtoms]),
    % reload module that we've just build
    {module, ?VICTIM} = code:load_binary(?VICTIM, Filename, Bin),
    code:purge(?VICTIM),
    % ensure it still works
    ?assertEqual(echo, power_shell:eval(?VICTIM, local_unexported, [echo])),
    % ensure new md5 is cached
    {state, #{?VICTIM := ModInfo1}} = sys:get_state(power_shell_cache),
    ?assertEqual(?VICTIM:module_info(md5), element(2, ModInfo1)),
    ok.

no_debug_info() ->
    [{doc, "Tests loading from source code (*.erl) due to missing debug_info chunk"}].

no_debug_info(_Config) ->
    % remove debug_info from power_shell_SUITE
    Filename = code:which(?VICTIM),
    {ok, ?VICTIM, Chunks} = beam_lib:all_chunks(Filename),
    %
    WithoutDbgInfo = lists:keydelete("Dbgi", 1, Chunks),
    {ok, Bin} = beam_lib:build_module(WithoutDbgInfo),
    ok = file:write_file(Filename, Bin),
    touch_file(Filename),
    % rewrite the *.beam file
    {module, ?VICTIM} = code:load_binary(?VICTIM, Filename, Bin),
    code:purge(?VICTIM),
    % keep it loaded, or unload - result should be the same
    ?assertEqual(echo, power_shell:eval(?VICTIM, local_unexported, [echo])),
    ok.

cover_compiled() ->
    [{doc, "Tests *.beam lookup for cover-compiled modules"}].

cover_compiled(_Config) ->
    cover_compile_test(fun () -> power_shell:eval(power_shell_default, proxy_fun, [erlang, node, 0]) end).

cover_compiled_direct() ->
    [{doc, "Tests *.beam lookup for cover-compiled modules, but without using gen_server cache"}].

cover_compiled_direct(_Config) ->
    FunMap = power_shell_cache:get_module(power_shell_default),
    cover_compile_test(fun () -> power_shell:eval(power_shell_default, proxy_fun, [erlang, node, 0], FunMap) end).

parse_transform() ->
    [{doc, "Tests that parse_transforms are applied when reading source code from *.erl file"}].

parse_transform(Config) ->
    Source =
        "-module(pt). -export([bar/0]). -include_lib(\"syntax_tools/include/merl.hrl\"). "
        "bar() -> inner(). "
        "inner() ->"
            "Tuple = ?Q(\"{foo, 42}\"),"
            "?Q(\"{foo, _@Number}\") = Tuple,"
            "Call = ?Q(\"foo:bar(_@Number)\"),"
            "erl_prettypr:format(merl:tree(Call)).",
    PrivPath = ?config(priv_dir, Config),
    true = code:add_path(PrivPath),
    Filename = filename:join(PrivPath, "pt.erl"),
    ok = file:write_file(Filename, Source),
    ?assertEqual("foo:bar(42)", power_shell:eval(pt, inner, [])),
    true = code:del_path(PrivPath),
    ok.

source_beam_select() ->
    [{doc, "Tests reloading from the latest timestamped file (*.beam or *.erl)"}].

source_beam_select(Config) ->
    Common = "-module(m1).\n-export([bar/0]).\nbar() -> inner().\ninner() ->",
    PrivPath = ?config(priv_dir, Config),
    true = code:add_path(PrivPath),
    SrcName = filename:join(PrivPath, "m1.erl"),
    ok = file:write_file(SrcName, Common ++ "bar."),
    touch_file(SrcName),
    %% write *.beam file that's younger
    Forms = [
        begin
            {ok, Line, _} = erl_scan:string(L),
            {ok, F} = erl_parse:parse_form(Line),
            F
        end || L <- string:lexemes(Common ++ "baz.", "\n")],
    {ok, m1, Bin} = compile:forms(Forms, [no_spawn_compiler_process, debug_info]),
    BeamName = filename:join(PrivPath, "m1.beam"),
    ok = file:write_file(BeamName, Bin),
    touch_file(BeamName),
    %% ensure new beam was selected
    ?assertEqual(baz, power_shell:eval(m1, inner, [])),
    %% overwrite source file, ensure old beam is still used
    ok = file:write_file(SrcName, Common ++ "ding."),
    touch_file(SrcName),
    ?assertEqual(baz, power_shell:eval(m1, inner, [])),
    %% now recompile and ensure new one is...
    {ok, m1} = compile:file(SrcName, [no_spawn_compiler_process, debug_info, {outdir, ?config(priv_dir, Config)}]),
    ?assertEqual(ding, power_shell:eval(m1, inner, [])),
    true = code:del_path(PrivPath),
    ok.

source_reload() ->
    [{doc, "Tests reloading source (*.erl) file"}].

source_reload(Config) ->
    Source = "-module(foo). -export([bar/0]). bar() -> inner(). inner() -> blah.",
    Source2 = "-module(foo). -export([bar/0]). bar() -> inner(). inner() -> boo.",
    PrivPath = ?config(priv_dir, Config),
    true = code:add_path(PrivPath),
    Filename = filename:join(PrivPath, "foo.erl"),
    ok = file:write_file(Filename, Source),
    touch_file(Filename),
    ?assertEqual(blah, power_shell:eval(foo, inner, [])),
    ok = file:write_file(Filename, Source2),
    ?assertEqual(boo, power_shell:eval(foo, inner, [])),
    true = code:del_path(PrivPath),
    ok.

no_beam() ->
    [{doc, "Tests loading from source code (*.erl) due to missing *.beam file"}].

no_beam(Config) ->
    {Filename, _} = ?config(beam_backup, Config),
    ?assertNotEqual(non_existing, Filename),
    file:delete(Filename),
    code:purge(?VICTIM),
    code:delete(?VICTIM),
    ?assertNot(code:is_loaded(?VICTIM)),
    ?assertEqual(echo, power_shell:eval(?VICTIM, local_unexported, [echo])),
    ?assertNot(code:is_loaded(?VICTIM)),
    ok.

on_load() ->
    [{doc, "Tests on_load executed for AST loaded from BEAM/source file"}].

on_load(Config) ->
    ok = application:set_env(power_shell, skip_on_load, false),
    Source =
        "-module(onl). -export([bar/0]). -on_load(side_effect/0). "
        "side_effect() -> ets:new(side, [named_table, public]), ok. bar() -> inner(). inner() -> ets:insert(side, {1, 2}).",
    PrivPath = ?config(priv_dir, Config),
    true = code:add_path(PrivPath),
    Filename = filename:join(PrivPath, "onl.erl"),
    ok = file:write_file(Filename, Source),
    power_shell_cache:get_module(onl),
    ?assertEqual(true, power_shell:eval(onl, inner, [])),
    true = code:del_path(PrivPath),
    %% cleanup side effect - remove ETS table
    true = ets:delete(side),
    ok = application:unset_env(power_shell, skip_on_load),
    ok.

no_on_load() ->
    [{doc, "Tests on_load skipped for modules without on_load function"}].

no_on_load(Config) ->
    ok = application:set_env(power_shell, skip_on_load, false),
    Source =
        "-module(no_onl). -export([bar/0]). "
        "bar() -> inner(). inner() -> true.",
    PrivPath = ?config(priv_dir, Config),
    true = code:add_path(PrivPath),
    Filename = filename:join(PrivPath, "no_onl.erl"),
    ok = file:write_file(Filename, Source),
    power_shell_cache:get_module(no_onl),
    ?assert(power_shell:eval(no_onl, inner, [])),
    true = code:del_path(PrivPath),
    ok = application:unset_env(power_shell, skip_on_load),
    ok.

broken_beam() ->
    [{doc, "Tests attempt to load a non-beam file"}].

broken_beam(Config) ->
    % make a fresh server
    ok = supervisor:terminate_child(power_shell_sup, ?SERVER),
    {ok, _Pid} = supervisor:restart_child(power_shell_sup, ?SERVER),
    %
    Filename = element(1, ?config(beam_backup, Config)),
    ?assertNotEqual(non_existing, Filename),
    ok = file:write_file(Filename, <<"FOR1 broken non-beam file">>),
    touch_file(Filename),
    ?assertMatch(
        {'EXIT', {{beam_lib, {error,beam_lib, {not_a_beam_file, _}}}, _}},
        catch power_shell:eval(?VICTIM, local_unexported, [echo])),
    ok.

wrong_module() ->
    [{doc, "Attempt to load module with wrong name"}].

wrong_module(Config) ->
    % make a fresh server
    ok = supervisor:terminate_child(power_shell_sup, ?SERVER),
    {ok, _Pid} = supervisor:restart_child(power_shell_sup, ?SERVER),
    %
    {Filename, _} = ?config(beam_backup, Config),
    ?assertNotEqual(non_existing, Filename),
    {ok, _} = file:copy(code:which(?MODULE), Filename),
    touch_file(Filename),
    ?assertMatch({'EXIT', {{badmatch, ?MODULE}, _}},
        catch power_shell_cache:get_module(?VICTIM)),
    ok.
