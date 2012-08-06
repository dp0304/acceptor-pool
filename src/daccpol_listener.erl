%%%-------------------------------------------------------------------
%%% @author dp <>
%%% @copyright (C) 2012, dp
%%% @doc
%%%
%%% @end
%%% Created :  6 Aug 2012 by dp <>
%%%-------------------------------------------------------------------
-module(daccpol_listener).

-behaviour(gen_server).

%% API

-export([start_link/2, stop/1,
	add_connection/3, remove_connection/2]). %% API.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).



-type pools() :: [{atom(), non_neg_integer()}].
-record(state, {
	req_pools = [] :: pools(),
	reqs_table :: ets:tid(),
	queue = undefined :: queue(),
	max_conns = undefined :: non_neg_integer(),
	proto_opts :: any()

}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(MaxConns, ProtoOpts) ->
	gen_server:start_link(?MODULE, [MaxConns, ProtoOpts],
		[{spawn_opt, [{priority, high}]}]).


stop(ServerPid) ->
	gen_server:call(ServerPid, stop).

%%向acceptor pool 加入一个链接
add_connection(ServerPid, Pool, ConnPid) ->
	gen_server:call(ServerPid, {add_connection, Pool, ConnPid},
		infinity).


remove_connection(ServerPid, ConnPid) ->
	gen_server:cast(ServerPid, {remove_connection, ConnPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
%% @private
init([MaxConns, ProtoOpts]) ->
	ReqsTable = ets:new(requests_table, [set, private]),
	Queue = queue:new(),
	{ok, #state{reqs_table=ReqsTable, max_conns=MaxConns,
		proto_opts=ProtoOpts, queue=Queue}}.

handle_call({add_connection, Pool, ConnPid}, From, State=#state{
		req_pools=Pools, reqs_table=ReqsTable,
		queue=Queue, max_conns=MaxConns
	      }) ->
	{NbConns, Pools2} = add_pid(ConnPid, Pool, Pools, ReqsTable),
	State2 = State#state{req_pools=Pools2},
	if	
		NbConns > MaxConns ->
			Queue2 = queue:in(From, Queue),
			{noreply, State2#state{queue=Queue2}};
		true ->
			{reply, ok, State2}
	end;
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.



handle_cast({remove_connection, ConnPid}, State=#state{
		req_pools=Pools, reqs_table=ReqsTable, queue=Queue}) ->
	{Pools2, Queue2} = remove_pid(ConnPid, Pools, ReqsTable, Queue),
	{noreply, State#state{req_pools=Pools2, queue=Queue2}};
handle_cast(_Msg, State) ->
	{noreply, State}.



handle_info({'DOWN', _Ref, process, Pid, _Info}, State=#state{
		req_pools=Pools, reqs_table=ReqsTable, queue=Queue}) ->
	{Pools2, Queue2} = remove_pid(Pid, Pools, ReqsTable, Queue),
	{noreply, State#state{req_pools=Pools2, queue=Queue2}};
handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
add_pid(ConnPid, Pool, Pools, ReqsTable) ->
	MonitorRef = erlang:monitor(process, ConnPid),
	ConnPid ! {shoot, self()},
	{NbConnsRet, Pools2} = case lists:keyfind(Pool, 1, Pools) of
		false ->
			{1, [{Pool, 1}|Pools]};
		{Pool, NbConns} ->
			NbConns2 = NbConns + 1,
			{NbConns2, [{Pool, NbConns2}|lists:keydelete(Pool, 1, Pools)]}
	end,
	ets:insert(ReqsTable, {ConnPid, {MonitorRef, Pool}}),
	{NbConnsRet, Pools2}.



remove_pid(Pid, Pools, ReqsTable, Queue) ->
	{MonitorRef, Pool} = ets:lookup_element(ReqsTable, Pid, 2),
	erlang:demonitor(MonitorRef, [flush]),
	{Pool, NbConns} = lists:keyfind(Pool, 1, Pools),
	Pools2 = [{Pool, NbConns - 1}|lists:keydelete(Pool, 1, Pools)],
	ets:delete(ReqsTable, Pid),
	case queue:out(Queue) of
		{{value, Client}, Queue2} ->
			gen_server:reply(Client, ok),
			{Pools2, Queue2};
		_ ->
			{Pools2, Queue}
	end.
