%%%-------------------------------------------------------------------
%%% @author dp <>
%%% @copyright (C) 2012, dp
%%% @doc
%%%  listener的创建
%%% @end
%%% Created :  6 Aug 2012 by dp <>
%%%-------------------------------------------------------------------
-module(daccpol_listener_sup).

-behaviour(supervisor).

%% API
-export([start_link/5]).

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

start_link(NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts) ->
	MaxConns = proplists:get_value(max_connections, TransOpts, 1024),
	{ok, SupPid} = supervisor:start_link(?MODULE, []),
	{ok, ListenerPid} = supervisor:start_child(SupPid,
		{daccpol_listener, {daccpol_listener, start_link, [MaxConns, ProtoOpts]},
		 permanent, 5000, worker, [daccpol_listener]}),
	{ok, ReqsPid} = supervisor:start_child(SupPid,
		{daccpol_requests_sup, {daccpol_requests_sup, start_link, []},
		 permanent, 5000, supervisor, [daccpol_requests_sup]}),
	{ok, _PoolPid} = supervisor:start_child(SupPid,
		{daccpol_acceptor_sup, {daccpol_acceptor_sup, start_link, [
			NbAcceptors, Transport, TransOpts,
			Protocol, ProtoOpts, ListenerPid, ReqsPid
		]}, permanent, 5000, supervisor, [daccpol_acceptor_sup]}),
	{ok, SupPid}.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
	{ok, {{one_for_all, 10, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
