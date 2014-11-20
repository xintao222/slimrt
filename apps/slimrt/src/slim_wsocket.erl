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

-include("slim_log.hrl").

-export([init/2,
		 websocket_handle/3,
		 websocket_info/3,
		 websocket_terminate/3]).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts, hibernate}.

% 
%% subscribe %{ticket}
%
websocket_handle({text, <<"subscribe ", STicket/binary>>}, Req, State) ->
    Ticket = slim_ticket:make(STicket),
	case slim_cm:lookup(Ticket) of
	Pid when is_pid(Pid) ->
		slim_client:subscribe(Pid, Ticket),
		Json = slim_json:encode([{status, ok}]),
		{reply, {text, Json}, Req, State, hibernate};
	undefined ->
		{shutdown, Req, State}
	end;

websocket_handle({pong, _}, Req, State) ->
	{ok, Req, State, hibernate};
	
websocket_handle(Data, Req, State) ->
	?ERROR("badata: ~p", [Data]),
	{shutdown, Req, State}.

websocket_info({ok, Packets}, Req, State) ->
	JSON = slim_json:pack(Packets),
	{reply, {text, JSON}, Req, State};

websocket_info(stop, Req, State) ->
	{shutdown, Req, State};

websocket_info(Info, Req, State) ->
	{shutdown, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

