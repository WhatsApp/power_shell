%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @doc
%%% Allows to inject proxy functions into existing modules, compiled
%%% with debug into. Added proxy functions are just redirecting
%%% calls to another module.
%%%
%%% This could be useful for environments where both shell_default
%%% and user_default are already used, and it's not possible to
%%% replace any of these.
%%% @end
%%%-------------------------------------------------------------------
-module(power_shell_default).
-author("maximfca@gmail.com").

%% API
-export([inject/2,
    eject/2]).

-export_type([inject_error/0]).

-type inject_error() ::
    already_loaded |
    no_abstract_code |
    {badmatch, module()} |
    {beam_lib, beam_lib:chnk_rsn()}.

%%====================================================================
%% API

%% @doc Extracts abstract code from WhereTo module, adds all
%%      exports from the module Mod into WhereTo, compiles
%%      result and loads it, replacing current WhereTo.
%%      This sequence injects proxies for all functions exported
%%      from Mod into WhereTo (ignoring compiler-generated
%%      module_info/0 and module_info/1).
%%      For example, if Mod exports eval/3, after calling
%%      power_shell_default:inject(Mod) resulting WhereTo
%%      contains following code:
%%  ```
%%  -export([eval/3]).
%%  eval(Arg1, Arg2, Arg3) ->
%%      Mod:eval(Arg1, Arg2, Arg3).'''
%%
%%      If there is no WhereTo module loaded, proxy just
%%      creates one from scratch.
%% @param WhereTo Module name to add proxy methods to
%% @param Mod Module name to proxy calls to

-spec inject(WhereTo :: module(), Mod :: module()) -> ok | {error, inject_error()}.
inject(WhereTo, Mod) when is_atom(WhereTo), is_atom(Mod) ->
    Filename = atom_to_list(Mod),
    case code:which(WhereTo) of
        Filename ->
            {error, already_loaded};
        NewFile when is_list(NewFile) ->
             case beam_lib:chunks(NewFile, [abstract_code]) of
                 {ok, {WhereTo, [{abstract_code, {_, Forms}}]}} ->
                     inject_impl(Forms, Filename, WhereTo, Mod);
                 {ok, {WhereTo, [{abstract_code,no_abstract_code}]}} ->
                     {error, no_abstract_code};
                 {ok, {ActualMod, _}} ->
                     {error, {badmatch, ActualMod}};
                 Error ->
                     {error, {beam_lib, Error}}
             end;
        non_existing ->
            inject_impl([{attribute,20,module,WhereTo}], Filename, WhereTo, Mod)
    end.

%% @doc Verifies that current WhereFrom has injected functions from
%%      Mod, and purges/deletes WhereFrom code from memory. Next
%%      time shell does ensure_loaded() for WhereFrom, an unmodified
%%      version is loaded from disk.
%% @param WhereFrom original module name, e.g. shell_default
%% @param Mod module that has been injected

-spec eject(WhereFrom :: module(), Mod :: module()) -> ok | {error, not_loaded}.
eject(WhereFrom, Mod) when is_atom(WhereFrom), is_atom(Mod) ->
    ModFile = atom_to_list(Mod),
    case code:is_loaded(WhereFrom) of
        {file, ModFile} ->
            % just purge/delete our version, so OTP loads
            % previous edition from dist
            code:purge(WhereFrom),
            true = code:delete(WhereFrom),
            ok;
        _ ->
            {error, not_loaded}
    end.

%%====================================================================
%% Internal functions

% Inserts an already reversed sequence of attributes and
%   functions in the middle of parsed AST.
% This essentially adds a few more exported functions to the
%   file, pretty much like compiler adds module_info().
insert_funs(Exports, Funs, Forms) ->
    {Attrs, Functions} = lists:splitwith(
        fun ({function, _, _, _, _}) ->
                false;
            (_) ->
                true
        end,
        Forms),
    Attrs ++ [Exports | Funs] ++ Functions.

% Generates abstract syntax tree for proxy function Mod:Fun/Arity.
proxy_fun(Mod, Fun, Arity) ->
    ProxyArgs = [{var, 1, list_to_atom("Arg" ++ integer_to_list(N))} ||
        N <- lists:seq(1, Arity)], % [{var,1,'T'}]
    {function, 1, Fun, Arity,
        [{clause, 1,
            ProxyArgs,
            [],
            [{call,1,
                {remote, 1, {atom, 1, Mod}, {atom, 1, Fun}},
                ProxyArgs}]}]}.

inject_impl(Forms, Filename, WhereTo, Mod) ->
    % pick exports from Mod (power_shell expected), removing OTP-added functions
    Forwards = Mod:module_info(exports) -- [{module_info, 0}, {module_info, 1}],
    % make AST out of exports
    Exports = {attribute, 1, export, Forwards},
    Funs = [proxy_fun(Mod, Fun, Arity) || {Fun, Arity} <- Forwards],
    Forms1 = insert_funs(Exports, Funs, Forms),
    % compile to beam
    {ok, App, Bin} = compile:forms(Forms1),
    % unstick if necessary
    StickBack = case code:is_sticky(WhereTo) of
                    true ->
                        code:unstick_mod(WhereTo);
                    false ->
                        false
                end,
    % load augmented code
    {module, WhereTo} = code:load_binary(App, Filename, Bin),
    % stick back if was unstuck
    if StickBack -> code:stick_mod(WhereTo); true -> ok end,
    % and immediately purge old code (race conditions are unlikely)
    code:purge(WhereTo),
    ok.
