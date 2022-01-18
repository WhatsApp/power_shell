%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @doc
%%% Used for testing power_shell:export/2,3 functions. It is not possible
%%%  to safely load and purge the power_shell_SUITE module itself when
%%%  running tests over the module, hence the need in this one.
%%% @end
%%%-------------------------------------------------------------------
-module(power_shell_export).
-author("maximfca@gmail.com").

%% API
-export([export_all/1]).

%%--------------------------------------------------------------------
%% IMPORTANT: THESE MUST NOT BE EXPORTED !!!

local_unexported(Echo) ->
    Echo.

local_never(Echo) ->
    Echo.

%% Kitchen sink to silence compiler in a good way (without suppressing warnings)
export_all(Arg) ->
    local_never(local_unexported(Arg)).