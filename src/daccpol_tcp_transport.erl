%%%-------------------------------------------------------------------
%%% @author dp <>
%%% @copyright (C) 2012, dp
%%% @doc
%%%  
%%% @end
%%% Created :  6 Aug 2012 by dp <>
%%%-------------------------------------------------------------------
-module(daccpol_tcp_transport).

-export([name/0, messages/0, listen/1, accept/1, recv/3, send/2, setopts/2,
	controlling_process/2, peername/1, close/1, sockname/1]).
-export([connect/3]).

%% @doc Name of this transport API, <em>tcp</em>.
-spec name() -> tcp.
name() -> tcp.

%% @doc Atoms used in the process messages sent by this API.
%%
%% They identify incoming data, closed connection and errors when receiving
%% data in active mode.
-spec messages() -> {tcp, tcp_closed, tcp_error}.
messages() -> {tcp, tcp_closed, tcp_error}.

%% @private
connect(Host, Port, Opts) when is_list(Host), is_integer(Port) ->
	gen_tcp:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}]).

%% @doc Setup a socket to listen on the given port on the local host.
%%
%% The available options are:
%% <dl>
%%  <dt>port</dt><dd>Mandatory. TCP port number to open.</dd>
%%  <dt>backlog</dt><dd>Maximum length of the pending connections queue.
%%   Defaults to 1024.</dd>
%%  <dt>ip</dt><dd>Interface to listen on. Listen on all interfaces
%%   by default.</dd>
%% </dl>
%%
%% @see gen_tcp:listen/2
-spec listen([{port, inet:port_number()} | {ip, inet:ip_address()}])
	-> {ok, inet:socket()} | {error, atom()}.
listen(Opts) ->
	{port, Port} = lists:keyfind(port, 1, Opts),
	Backlog = proplists:get_value(backlog, Opts, 1024),
	   Packet = proplists:get_value(packet, Opts, 0),
    Active = proplists:get_value(active, Opts, false),
    
	ListenOpts = [binary, {active, Active},
		{backlog, Backlog}, {packet, Packet}, {reuseaddr, true}],
	
	gen_tcp:listen(Port, ListenOpts).


accept(LSocket) ->
	gen_tcp:accept(LSocket).

%% @doc Receive a packet from a socket in passive mode.
%% @see gen_tcp:recv/3
-spec recv(inet:socket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	gen_tcp:recv(Socket, Length, Timeout).

%% @doc Send a packet on a socket.
%% @see gen_tcp:send/2
-spec send(inet:socket(), iolist()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	gen_tcp:send(Socket, Packet).

%% @doc Set one or more options for a socket.
%% @see inet:setopts/2
-spec setopts(inet:socket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	inet:setopts(Socket, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see gen_tcp:controlling_process/2
-spec controlling_process(inet:socket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	gen_tcp:controlling_process(Socket, Pid).

%% @doc Return the address and port for the other end of a connection.
%% @see inet:peername/1
-spec peername(inet:socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
	inet:peername(Socket).

%% @doc Close a TCP socket.
%% @see gen_tcp:close/1
-spec close(inet:socket()) -> ok.
close(Socket) ->
	gen_tcp:close(Socket).

%% @doc Get the local address and port of a socket
%% @see inet:sockname/1
-spec sockname(inet:socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
	inet:sockname(Socket).
