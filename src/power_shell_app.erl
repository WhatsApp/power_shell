%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @doc Application bootstrap. If shell_default configuration
%%      variable is set to true during start, power_shell is
%%      automatically injected into current shell_default.
%%% @private
%%%-------------------------------------------------------------------

-module(power_shell_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, integrate/1]).

%%====================================================================
%% API

start(_StartType, _StartArgs) ->
    % fail if shell_default is already loaded
    % register as default shell, if asked for
    case application:get_env(shell_integration) of
        {ok, shell_default} ->
            integrate(shell_default);
        {ok, user_default} ->
            integrate(user_default);
        undefined ->
            integrate(user_default)
    end,
    %
    % we have to run a no-op supervisor to report
    %   application health.
    % otherwise, just use it as a library, do not
    %   start the app, it is good enough!
    power_shell_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    % unload shell_default when stopping - via supervisor
    power_shell_default:eject(shell_default, power_shell),
    power_shell_default:eject(user_default, power_shell),
    ok.

%%====================================================================
%% API

%% @doc Implements user_default or shell_default integration.
%%      For shell_default, proxy injection technique is used.
%%      For user_default, if there is no other user_default module
%%      loaded, just rename power_shell to user_default and load as
%%      binary.
-spec integrate(user_default | shell_default) -> ok | {error, code:loaded_ret_atoms()}.

integrate(shell_default) ->
    ok = power_shell_default:inject(shell_default, power_shell);
integrate(user_default) ->
    ok = power_shell_default:inject(user_default, power_shell).
