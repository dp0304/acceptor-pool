%%%-------------------------------------------------------------------
%%% @author dp <>
%%% @copyright (C) 2012, dp
%%% @doc
%%% 接入的api
%%% @end
%%% Created :  6 Aug 2012 by dp <>
%%%-------------------------------------------------------------------
-module(daccpol).

%% API
-export([start_listener/6, stop_listener/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%启动监听的
start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts)
		when is_integer(NbAcceptors) andalso is_atom(Transport)
		andalso is_atom(Protocol) ->
	supervisor:start_child(daccpol_sup, {{daccpol_listener_sup, Ref}, {daccpol_listener_sup, start_link, [
		NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
	]}, permanent, 5000, supervisor, [daccpol_listener_sup]}).

%%停止监听
stop_listener(Ref) ->
	case supervisor:terminate_child(daccpol_sup, {daccpol_listener_sup, Ref}) of
		ok ->
			supervisor:delete_child(daccpol_sup, {daccpol_listener_sup, Ref});
		{error, Reason} ->
			{error, Reason}
	end.



