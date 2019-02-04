%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @doc
%%% Loads abstract module code, either from debug_info *.beam chunk,
%%% or from *.erl source file.
%%%
%%% Caching server may not be started, in this case get_module() does
%%% not use caching logic and creates AST every call.
%%% If loaded module md5 hash is changed, or *.beam/*.erl file modification
%%% date is different from the last call, module AST gets reloaded from the
%%% corresponding file.
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

-define(SERVER, ?MODULE).

-include_lib("kernel/include/file.hrl").

-ifdef(OTP_RELEASE).
-include_lib("kernel/include/logger.hrl").
-else.
-define(LOG_WARNING(A,B,C), _ = B).
-endif.

-type function_clause() :: {function, integer(), atom(), arity(), {clauses, erl_eval:clauses()}}.

-type function_map() :: #{{atom(), arity()} => function_clause()}.

% For every module, store:
%   * hash of the loaded code, if module is loaded
%   * file name used to access the file
%   * modification date of the file
% For file-based interpreter, it could be either BEAM or *.erl.
-record(module_data, {
    hash = undefined :: binary() | undefined,
    filename = undefined :: string() | undefined,
    mtime = undefined :: non_neg_integer() | undefined,
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
        {error, Reason} ->
            erlang:error(Reason)
    catch
        exit:{noproc, _} ->
            #module_data{fun_map = FunMap} = decompile(Mod, is_loaded(Mod)),
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
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
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
%%
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
    %
    case maps:get(Mod, Modules, #module_data{}) of

        #module_data{hash = Hash} when
            (Hash =:= undefined andalso Loaded =:= true);
            (Hash =/= undefined andalso Loaded =:= false) ->
            % BEAM has been either loaded or unloaded since last call,
            %   need to rescan
            reply_recompile(Mod, Loaded, State);

        #module_data{hash = undefined, filename = Filename, mtime = MTime} = ModData ->
            % check if file has changed, and reload if it was
            case file_changed(Filename, MTime) of
                true ->
                    reply_recompile(Mod, Loaded, State);
                false ->
                    {reply, {ok, ModData}, State}
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
%%
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
    try decompile(Mod, Loaded) of
        #module_data{} = ModInfo ->
            {reply, {ok, ModInfo}, State#state{modules = maps:put(Mod, ModInfo, Modules)}}
    catch
        error:Reason ->
            {reply, {error, Reason}, State}
    end.

%%%===================================================================

extract_hash(Mod, true) ->
    Mod:module_info(md5);
extract_hash(_, false) ->
    undefined.

extract_funs(Mod, Forms) ->
    %% TODO: implement source code loading, will have to run lint & records
    % expansion
    %case erl_lint:module(Forms) of
    %    {ok, _Warnings} ->
    %        ok; % ignore warnings for now - but maybe print?
    %    {error, Errors, Warnings} ->
    %        erlang:error({erl_lint, [{errors, Errors}, {warnings, Warnings}]})
    %end,
    %%
    %Forms2 =
    %    case HasRecs of
    %        false -> Forms;
    %        true  -> erl_expand_records:module(Forms, [])
    %    end,
    select_funs(Mod, Forms).

select_funs(Mod, Forms) ->
    lists:foldl(
        fun ({function, _, FunName, Arity, _} = Body, FunMap) ->
                maps:put({FunName, Arity}, Body, FunMap);
            % following can only happen if we use source *.erl code.
            % When it's from debug_info, beam_lib:chunks() does the
            %   detection routine.
            %({attribute, _, module, ActualMod}, _) when ActualMod =/= Mod ->
            %    erlang:error({badmatch, ActualMod});
            (_, FunMap) ->
                FunMap
        end, #{module => Mod}, Forms).

% may error enoent for missing code
%           {enoent, abstract_code} for missing debug_info
%           {badmatch, ActualName} for wrong module name
%           {beam_lib, Reason} for beam_lib error
-spec decompile(Module :: module(), Loaded :: boolean()) -> module_data().
decompile(Mod, Loaded) when is_atom(Mod) ->
    case code:which(Mod) of
        Filename when is_list(Filename) ->
            % race condition: code:which() may find the file being
            %   deleted at this very moment. Just don't care,
            %   this is a weird enough situation not to handle it.
            % Another method would be to compare md5 embedded in the
            %   chunks, but that feels too much for a simple piece
            %   of code
            {ok, #file_info{mtime = MTime}} =
                file:read_file_info(Filename, [{time, posix}]),
            case beam_lib:chunks(Filename, [abstract_code]) of
                {ok, {Mod, [{abstract_code, {_, Forms}}]}} ->
                    #module_data{
                        hash = extract_hash(Mod, Loaded),
                        fun_map = extract_funs(Mod, Forms),
                        filename = Filename,
                        mtime = MTime
                    };
                {ok,{Mod, [{abstract_code,no_abstract_code}]}} ->
                    % no debug_info, but maybe there is a source file
                    %   lying around? Yes, there is a chance this file
                    %   is different from compiled BEAM version.
                    %   This could be detected by comparing md5 of
                    %   the compiled version, TODO: actually compare
                    erlang:error(enoent);
                {ok, {ActualMod, _}} ->
                    erlang:error({badmatch, ActualMod});
                Error ->
                    erlang:error({beam_lib, Error})
            end;
        non_existing ->
            % look for source file, we might be lucky to find one
            %   and parse it instead of BEAM debug_info chunks
            erlang:error(enoent);
        preloaded ->
            % preloaded modules are not to be decompiled,
            %   so just report it
            erlang:error(preloaded)
    end.

file_changed(Filename, MTime) ->
    case file:read_file_info(Filename, [{time, posix}]) of
        {ok, #file_info{mtime = Time}} when Time =:= MTime ->
            false;
        _  ->
            true
    end.

% Joe Armstrong mentioned this in the book:
% get_cached(Mod, #{Mod := ModData} = ModMap) ->
%
% However, compiler does not like it, considering Mod unbound during
%   map key matching. So yes, do an ugly case-hack with maps:get()
