%%%-------------------------------------------------------------------
%%% @author dp <>
%%% @copyright (C) 2012, dp
%%% @doc
%%%
%%% @end
%%% Created :  6 Aug 2012 by dp <>
%%%-------------------------------------------------------------------
-module(daccpol_acceptor_sup).

-behaviour(supervisor).

%% API
-export([start_link/7]).

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
start_link(NbAcceptors, Transport, TransOpts,
		Protocol, ProtoOpts, ListenerPid, ReqsPid) ->
	supervisor:start_link(?MODULE, [NbAcceptors, Transport, TransOpts,
		Protocol, ProtoOpts, ListenerPid, ReqsPid]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([NbAcceptors, Transport, TransOpts,
		Protocol, ProtoOpts, ListenerPid, ReqsPid]) ->
	{ok, LSocket} = Transport:listen(TransOpts),
	Procs = [{{acceptor, self(), N}, {daccpol_acceptor, start_link, [
				LSocket, Transport, Protocol, ProtoOpts,
				ListenerPid, ReqsPid
      ]}, permanent, brutal_kill, worker, []}
		|| N <- lists:seq(1, NbAcceptors)],
	{ok, {{one_for_one, 10, 10}, Procs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
