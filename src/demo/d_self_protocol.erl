%%%-------------------------------------------------------------------
%%% @author dp <>
%%% @copyright (C) 2012, dp
%%% @doc
%%%  echo服务器
%%% @end
%%% Created :  7 Aug 2012 by dp <>
%%%-------------------------------------------------------------------
-module(d_self_protocol).

-export([start_link/4, init/2,terminate/0]).

-spec start_link(pid(), ssl:sslsocket(), module(), []) -> {ok, pid()}.
start_link(_ListenerPid, Socket, Transport, []) ->
    Pid = spawn_link(?MODULE, init, [Socket, Transport]),
    {ok, Pid}.



init(Socket, Transport) ->
    recv(Socket, Transport),
    ok.

terminate() ->
    ok.



recv(Socket, Transport)->
    case Transport:recv(Socket,0,10000) of
	{ok,Data}->

	    io:format("receive data~n"),
	    Transport:send(Socket,Data),
	    recv(Socket,Transport);
	{error,timeout}->ok;
	{error,closed}->ok
    end.
