%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @private
%%%-------------------------------------------------------------------
-module(power_shell_cache_SUITE).
-author("maximfca@gmail.com").

%%--------------------------------------------------------------------

-export([all/0,
    suite/0, init_per_suite/1, end_per_suite/1]).

-export([get_module/0, get_module/1,
    bad_calls/0, bad_calls/1,
    start_stop/0, start_stop/1,
    wrong_module/0, wrong_module/1,
    cache_md5_check/0, cache_md5_check/1,
    not_loaded/0, not_loaded/1,
    no_debug_info/0, no_debug_info/1,
    cover_compiled/0, cover_compiled/1,
    source_reload/0, source_reload/1,
    no_beam/0, no_beam/1,
    broken_beam/0, broken_beam/1]).

%% Common Test headers
-include_lib("common_test/include/ct.hrl").

%% Include stdlib header to enable ?assert() for readable output
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").

-define(SERVER, power_shell_cache).
-define(VICTIM, power_shell_SUITE).

suite() ->
    [{timetrap,{seconds,30}}].

all() ->
    [get_module, bad_calls, start_stop, cache_md5_check, not_loaded,
        no_debug_info, cover_compiled, source_reload, no_beam, broken_beam, wrong_module].

init_per_suite(Config) ->
    application:ensure_started(power_shell),
    ok = application:stop(power_shell),
    ok = application:set_env(power_shell, cache_code, true),
    ok = application:start(power_shell),
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
    ok = application:start(power_shell),
    Config.

touch_file(Filename) ->
    {ok, FileInfo} = file:read_file_info(Filename, [{time, posix}]),
    ok = file:write_file_info(Filename,
        FileInfo#file_info{mtime = FileInfo#file_info.mtime + 1}, [{time, posix}]).

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
    % power_shell itself should be cover-compiled, ensure it is so
    ?assertEqual(cover_compiled, code:which(power_shell)),
    % ensure decompilation works on cover-compiled files
    FunMap = power_shell_cache:get_module(power_shell),
    ?assertNotEqual(error, maps:find({do_apply, 2}, FunMap)),
    % "we need to go deeper" - now within 4 apply levels
    ?assertEqual(node(), power_shell:eval(power_shell, do_apply, [{erlang, node}, []])).

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
