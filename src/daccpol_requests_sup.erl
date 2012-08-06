%%%-------------------------------------------------------------------
%%% @author dp <>
%%% @copyright (C) 2012, dp
%%% @doc
%%%  这里会回调自定义的协议模块
%%% @end
%%% Created :  6 Aug 2012 by dp <>
%%%-------------------------------------------------------------------
-module(daccpol_requests_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_request/5]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%回调自定义协议模块
start_request(ListenerPid, Socket, Transport, Protocol, Opts) ->
	Protocol:start_link(ListenerPid, Socket, Transport, Opts).
%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


init([]) ->
	{ok, {{simple_one_for_one, 0, 1}, [{?MODULE, {?MODULE, start_request, []},
		temporary, brutal_kill, worker, [?MODULE]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
