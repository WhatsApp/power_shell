%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @doc
%%% Loads abstract module code, either from debug_info *.beam chunk,
%%% or from *.erl source file.
%%%
%%% Caching server may not be started, in this case get_module() does
%%% not use caching logic and creates AST every call.
%%% If loaded module md5 hash is changed, or *.erl file modification
%%% date is different from the last call, module AST gets reloaded.
%%% @end
%%%-------------------------------------------------------------------
-module(power_shell_cache).
-author("maximfca@gmail.com").

-behaviour(gen_server).

-export([get_module/1]).

%% Decompiled/loaded code cache server
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2]).

-export_type([function_map/0]).

-define(SERVER, ?MODULE).

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").

-type function_clause() :: {function, integer(), atom(), arity(), {clauses, [erl_parse:abstract_clause()]}}.

-type function_map() :: #{{atom(), arity()} => function_clause()}.

% For every module, store:
%   * loaded code md5 hash (undefined when *.beam is not loaded)
%   * file name used to access the file, *.beam or *.erl (could also be 'preloaded')
%   * modification date of the file (*.beam or source)
% For file-based interpreter, it could be either BEAM or *.erl.
-record(module_data, {
    hash = undefined :: binary() | undefined,
    filename = undefined :: string() | undefined,
    mtime = undefined :: file:date_time() | undefined,
    fun_map = #{} :: function_map()
}).

-type module_data() :: #module_data{}.

% Code cache state
-record(state, {
    modules = #{} :: #{module() => module_data()}
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Extracts AST of all functions defined in Mod. Caches this
%%      if power_shell_cache server is running.
-spec get_module(Mod :: module()) -> function_map().
get_module(Mod) ->
    try gen_server:call(?MODULE, {get, Mod}) of
        {ok, #module_data{fun_map = FunMap}} ->
            FunMap;
        {error, Reason, Stack} ->
            erlang:raise(error, Reason, Stack)
    catch
        exit:{noproc, _} ->
            #module_data{fun_map = FunMap} =
                case decompile(Mod, is_loaded(Mod), undefined) of
                    #module_data{} = ModData ->
                        ModData;
                    need_cover ->
                        % requested module was cover-compiled
                        decompile(Mod, is_loaded(Mod), decompile(cover, true, undefined))
                end,
            FunMap
    end.

%% @doc
%% Starts power_shell_cache server that keeps track of already
%%  decompiled modules.
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).

% Can't match Mod and #modules{} map for the module,
%   even though Joe Armstrong writes this example
%   in the book. So use maps:get()...
handle_call({get, Mod}, _From, #state{modules = Modules} = State) ->
    %
    Loaded = is_loaded(Mod),
    case maps:get(Mod, Modules, error) of

        error ->
            reply_recompile(Mod, Loaded, State);

        #module_data{hash = Hash} when
            (Hash =:= undefined andalso Loaded =:= true);
            (Hash =/= undefined andalso Loaded =:= false) ->
            % BEAM has been either loaded or unloaded since last call,
            %   need to rescan
            reply_recompile(Mod, Loaded, State);

        #module_data{hash = undefined, filename = Filename, mtime = MTime} = ModData ->
            % loaded from file, need to check modification time
            case filelib:last_modified(Filename) of
                MTime ->
                    {reply, {ok, ModData}, State};
                _AnotherTime ->
                    reply_recompile(Mod, Loaded, State)
            end;

        #module_data{hash = Hash} = ModData ->
            % check if loaded BEAM has a different md5
            case Mod:module_info(md5) of
                Hash ->
                    {reply, {ok, ModData}, State};
                AnotherHash when is_binary(AnotherHash) ->
                    reply_recompile(Mod, Loaded, State)
            end
    end;

handle_call(Request, _From, State) ->
    {reply, {error, {undef, Request}}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(Request, State) ->
    % next line cannot be split into several lines, otherwise coverage will
    %   report missing lines (and those actually are missing, but we don't care now)
    ?LOG_WARNING("** Undefined handle_cast in ~p~n** Unhandled message: ~tp~n", [?MODULE, Request], #{domain=>[otp], error_logger => #{tag=>warning_msg}}),
    {noreply, State}.

%%%===================================================================
%%% Internal functions

is_loaded(Mod) ->
    code:is_loaded(Mod) =/= false.

reply_recompile(Mod, Loaded, #state{modules = Modules} = State) ->
    try decompile(Mod, Loaded, maps:get(cover, Modules, undefined)) of
        need_cover ->
            % requested module was cover-compiled, and now we need to decompile 'cover'
            %   to allow further progress
            ModInfo = decompile(cover, true, undefined),
            reply_recompile(Mod, Loaded, State#state{modules = maps:put(cover, ModInfo, Modules)});
        #module_data{} = ModInfo ->
            {reply, {ok, ModInfo}, State#state{modules = maps:put(Mod, ModInfo, Modules)}}
    catch
        error:Reason:Stack ->
            {reply, {error, Reason, Stack}, State}
    end.

%%%===================================================================

extract_hash(Mod, true) ->
    Mod:module_info(md5);
extract_hash(_, false) ->
    undefined.

select_funs(Mod, Forms) ->
    lists:foldl(
        fun ({function, _, FunName, Arity, _} = Body, FunMap) ->
                maps:put({FunName, Arity}, Body, FunMap);
            % following can only happen if we use source *.erl code.
            % When it's from debug_info, beam_lib:chunks() does the
            %   detection routine.
            %({attribute, _, module, ActualMod}, _) when ActualMod =/= Mod ->
            %    erlang:error({badmatch, ActualMod});
            ({attribute, _, on_load, {OnLoad, 0}}, FunMap) ->
                maps:put(on_load, OnLoad, FunMap);
            (_, FunMap) ->
                FunMap
        end, #{module => Mod}, Forms).

% may error enoent for missing beam & source files
%           {enoent, abstract_code} for missing debug_info and source file
%           {badmatch, ActualName} for wrong module name
%           {beam_lib, Reason} for beam_lib error
-spec decompile(Module :: module(), Loaded :: boolean(), Cover :: function_map() | undefined | need_cover) ->
    module_data() | need_cover.
decompile(Mod, Loaded, Cover) when is_atom(Mod) ->
    case code:get_object_code(Mod) of
        {Mod, Binary, Filename} ->
            load_binary(Mod, Binary, Filename, Loaded, Cover);
        error ->
            % look for source file, we might be lucky to find one
            %   and parse it instead of BEAM debug_info chunks
            load_erl(Mod, code:where_is_file(atom_to_list(Mod) ++ ".erl"), Cover)
    end.

load_binary(Mod, Binary, Filename, Loaded, Cover) ->
    case beam_lib:chunks(Binary, [abstract_code]) of
        {ok, {Mod, [{abstract_code, {_, Forms}}]}} ->
            Expanded = erl_expand_records:module(Forms, [strict_record_tests]),
            maybe_cover(Expanded, code:which(Mod), Mod, Filename, Loaded, Cover);
        {ok,{Mod, [{abstract_code,no_abstract_code}]}} ->
            % no debug_info, but maybe there is a source file
            %   lying around? Yes, there is a chance this file
            %   is different from compiled BEAM version.
            %   This could be detected by comparing md5 of
            %   the compiled version, TODO: actually compare
            {ok, Source} = filelib:find_source(code:which(Mod)),
            load_erl(Mod, Source, Cover);
        {ok, {ActualMod, _}} ->
            erlang:error({badmatch, ActualMod});
        Error ->
            erlang:error({beam_lib, Error})
    end.

load_erl(Mod, Filename, Cover) when is_list(Filename) ->
    {ok, Mod, Binary} = compile:file(Filename, [binary, debug_info]),
    load_binary(Mod, Binary, Filename, false, Cover);
load_erl(_, _, _) ->
    error(enoent).

maybe_cover(_Expanded, cover_compiled, _Mod, _Filename, _Loaded, undefined) ->
    need_cover;
maybe_cover(Expanded, cover_compiled, Mod, Filename, Loaded, #module_data{fun_map = FunMap}) ->
    {attribute, _, file, {MainFile, _}} = lists:keyfind(file, 3, Expanded),
    Covered = cover_unsafe_internal_compile(Mod, is_modern_cover(), Expanded, MainFile, FunMap),
    maybe_cover(Covered, undefined, Mod, Filename, Loaded, undefined);
maybe_cover(Expanded, _File, Mod, Filename, Loaded, _Cover) ->
    FunMap = select_funs(Mod, Expanded),
    #module_data{
        hash = extract_hash(Mod, Loaded),
        fun_map = maybe_onload(
            %% compatibility behaviour: power_shell was skipping on_load
            application:get_env(power_shell, skip_on_load, true),
            Mod,
            maps:get(on_load, FunMap, false),
            FunMap),
        filename = Filename,
        mtime = filelib:last_modified(Filename)
    }.

maybe_onload(false, Mod, FunName, FunMap) when is_map_key(on_load, FunMap) ->
    ok = power_shell:eval(Mod, FunName, [], FunMap),
    FunMap;
maybe_onload(_Skip, _Mod, _, FunMap) ->
    FunMap.

is_modern_cover() ->
    Vsn = case application:get_key(tools, vsn) of
              {ok, Vsn0} ->
                  Vsn0;
              undefined ->
                  ok = application:load(tools),
                  {ok, Vsn0} = application:get_key(tools, vsn),
                  Vsn0
          end,
    [Major, Minor | _] = [list_to_integer(V) || V <- string:lexemes(Vsn, ".")],
    Major >= 3 andalso Minor >= 2.

%% 'cover', OTP21 or below
cover_unsafe_internal_compile(Mod, false, Expanded, MainFile, FunMap) ->
    {Covered, _Vars} = power_shell:eval(cover, transform, [Expanded, Mod, MainFile], FunMap),
    Covered;

%% 'cover' from tools-3.2 and above: uses 'counters'
cover_unsafe_internal_compile(Mod, true, Expanded, MainFile, FunMap) ->
    Vars0 = {vars, Mod, [], undefined, undefined, undefined, undefined, undefined, undefined, false},
    {ok, MungedForms0, _Vars} = power_shell:eval(cover, transform_2, [Expanded, [], Vars0, MainFile, on], FunMap),
    %{Covered1, _Vars} = power_shell:eval(cover, transform, [Expanded, power_shell, MainFile, true], FunMap),
    Cref = ets:lookup_element(cover_internal_mapping_table, {counters, Mod}, 2),
    AbstrCref = power_shell:eval(cover, cid_to_abstract, [Cref], FunMap),
    power_shell:eval(cover, patch_code1, [MungedForms0, {local_only,AbstrCref}], FunMap).
