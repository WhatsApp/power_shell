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
    no_beam/0, no_beam/1,
    broken_beam/0, broken_beam/1]).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, power_shell_cache).
-define(VICTIM, power_shell_SUITE).

suite() ->
    [{timetrap,{seconds,30}}].

all() ->
    [get_module, bad_calls, start_stop, cache_md5_check, not_loaded, no_debug_info, no_beam, broken_beam, wrong_module].

init_per_suite(Config) ->
    application:ensure_started(power_shell),
    ok = application:stop(power_shell),
    ok = application:set_env(power_shell, cache_code, true),
    ok = application:start(power_shell),
    % make a backup of power_shell_SUITE.beam
    Path = code:which(?VICTIM),
    % into private data dir
    PrivPath = proplists:get_value(priv_dir, Config),
    BackupPath = [PrivPath, filename:basename(Path)],
    file:copy(Path, BackupPath),
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
    ok = gen_server:stop(?SERVER).

not_loaded() ->
    [{doc, "Tests interpretation of not yet loaded *.beam"}].

not_loaded(_Config) ->
    code:delete(?VICTIM),
    false = code:is_loaded(?VICTIM),
    %
    ?assertEqual(echo, power_shell:eval(?VICTIM, local_unexported, [echo])),
    %
    ?assertNot(code:is_loaded(?VICTIM)),
    ok.

cache_md5_check() ->
    [{doc, "Test cache reloading BEAM from dist when in-memory md5 changes"}].

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
    % rewrite the *.beam file
    {module, ?VICTIM} = code:load_binary(?VICTIM, Filename, Bin),
    code:purge(?VICTIM),
    % keep it loaded, or unload - result should be the same
    % TODO: implement *.erl file loading
    ?assertEqual({'EXIT', {undef,[{?VICTIM,local_unexported,[echo],[]}]}},
        catch power_shell:eval(?VICTIM, local_unexported, [echo])),
    ok.

no_beam() ->
    [{doc, "Tests loading from source code (*.erl) due to missing *.beam file"}].

no_beam(_Config) ->
    % remove debug_info from power_shell_SUITE
    Filename = code:which(?VICTIM),
    file:delete(Filename),
    code:purge(?VICTIM),
    code:delete(?VICTIM),
    false = code:is_loaded(?VICTIM),
    % TODO: implement *.erl file loading
    ?assertEqual({'EXIT', {undef,[{?VICTIM,local_unexported,[echo],[]}]}},
        catch power_shell:eval(?VICTIM, local_unexported, [echo])),
    %
    ?assertNot(code:is_loaded(?VICTIM)),
    ok.

broken_beam() ->
    [{doc, "Tests attempt to load a non-beam file"}].

broken_beam(Config) ->
    Filename = element(1, proplists:get_value(beam_backup, Config)),
    ?assertNotEqual(non_existing, Filename),
    ok = file:write_file(Filename, <<"FOR1 broken non-beam file">>),
    ?assertMatch(
        {'EXIT', {{beam_lib, {error,beam_lib, {not_a_beam_file, _}}}, _}},
        catch power_shell:eval(?VICTIM, local_unexported, [echo])),
    ok.

wrong_module() ->
    [{doc, "Attempt to load module with wrong name"}].

wrong_module(Config) ->
    Filename = element(1, proplists:get_value(beam_backup, Config)),
    ?assertNotEqual(non_existing, Filename),
    {ok, _} = file:copy(code:which(?MODULE), Filename),
    ?assertMatch({'EXIT', {{badmatch, ?MODULE}, _}},
        catch power_shell_cache:get_module(?VICTIM)),
    ok.
