%%%-------------------------------------------------------------------
%%% @author dp <>
%%% @copyright (C) 2012, dp
%%% @doc
%%%
%%% @end
%%% Created :  6 Aug 2012 by dp <>
%%%-------------------------------------------------------------------
-module(daccpol_acceptor).

%% API
-export([start_link/6]). 
-export([acceptor/6]). 

%%%===================================================================
%%% API
%%%===================================================================
start_link(LSocket, Transport, Protocol, Opts,
		ListenerPid, ReqsSup) ->
	Pid = spawn_link(?MODULE, acceptor,
		[LSocket, Transport, Protocol, Opts, ListenerPid, ReqsSup]),
	{ok, Pid}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
acceptor(LSocket, Transport, Protocol, Opts, ListenerPid, ReqsSup) ->
	Res = case Transport:accept(LSocket) of
		{ok, CSocket} ->
			{ok, Pid} = supervisor:start_child(ReqsSup,
				[ListenerPid, CSocket, Transport, Protocol, Opts]),
			Transport:controlling_process(CSocket, Pid),
			daccpol_listener:add_connection(ListenerPid,
				default, Pid);
		{error, _Reason} ->
			ok
	end,
	case Res of
		_ ->
			?MODULE:acceptor(LSocket, Transport, Protocol,
				Opts, ListenerPid, ReqsSup)
	
	end.
