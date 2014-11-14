%%-----------------------------------------------------------------------------
%% Copyright (c) 2014, Feng Lee <feng.lee@slimchat.io>
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%------------------------------------------------------------------------------

-module(slim_wsocket).

-author('feng.lee@slimchat.io').

-include("slimpp.hrl").

-export([init/2,
		 websocket_handle/3,
		 websocket_info/3,
		 websocket_terminate/3]).

%Websocket long connect
-define(WSOCKET, "/wsocket").

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts, hibernate}.

% 
%% subscribe ${domain} %{ticket}
%
websocket_handle({text, <<"subscribe ", Msg/binary>>}, Req, State) ->
	[Domain, STicket] = binary:split(Msg, [<<" ">>]),
    Ticket = #slim_ticket{class=Class, name=Name}
		= slim_ticket:make(STicket),
	UserOid = slim_oid:make(Class, Domain, Name),
	case slim_router:lookup(UserOid) of
	[Route] ->
		slim_endpoint:subscribe(Route#slim_route.pid, Ticket, self()),
		{reply, {text, slim_json:pack([])}, Req, State, hibernate};
	[] ->
		?ERROR("client not found, ticket: ~p", [Ticket]),
		{shutdown, Req, State}
	end;

websocket_handle({pong, _}, Req, State) ->
	{ok, Req, State, hibernate};
	
websocket_handle(Data, Req, State) ->
	%?ERROR("bad data: ~p", [Data]),
	{shutdown, Req, State}.

websocket_info({ok, Packets}, Req, State) ->
	JSON = slim_json:pack(lists:reverse(Packets)),
	{reply, {text, JSON}, Req, State};

websocket_info(stop, Req, State) ->
	{shutdown, Req, State};

websocket_info(Info, Req, State) ->
	{shutdown, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

