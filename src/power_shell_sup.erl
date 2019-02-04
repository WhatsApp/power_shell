%% @private
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @doc
%%% power_shell top level supervisor.
%%% When started, supervisor checks cache_code application configuration
%%%  variable, and in case it's set to true, power_shell_cache server
%%%  is started to allow abstract code caching.
%%% @end
%%%-------------------------------------------------------------------

-module(power_shell_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks

init([]) ->
    {ok, {#{},
        case application:get_env(cache_code) of
            {ok, true} ->
                [power_shell_cache_child_spec()];
            undefined ->
                []
        end
    }}.

%%====================================================================
%% Internal functions

power_shell_cache_child_spec() ->
    #{id => power_shell_cache,
        start => {power_shell_cache, start_link, []}, modules => [power_shell_cache]}.
