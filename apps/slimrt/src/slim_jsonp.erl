%%%----------------------------------------------------------------------
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

-module(slim_jsonp).

-author('feng.lee@slimchat.io').

-include("slimrt.hrl").

-import(lists, [reverse/1]).

-behaviour(cowboy_loop_handler).
 
-export([init/3, 
		info/3, 
		terminate/3]).

-record(state, {callback = <<>>, ticket, endpoint}).

init({tcp, http}, Req, _Opts) ->
	{Params, Req1} = cowboy_req:qs_vals(Req),
	?INFO("Jsonp request: ~p", [Params]),
    Domain = g(<<"domain">>, Params),
    Callback = g(<<"callback">>, Params, <<>>), 
    Ticket = #nextalk_ticket{class=Class, name=Name} = 
		nextalk_ticket:make(g(<<"ticket">>, Params)),
	Oid = nextalk_oid:make(Class, Domain, Name),
	case nextalk_router:lookup(Oid) of
	[Route] ->
		Endpoint = Route#nextalk_route.pid,
		State = #state{callback = Callback, ticket = Ticket, endpoint = Endpoint},
		nextalk_endpoint:subscribe(poll, Endpoint, Ticket, self()),
		{loop, Req1, State, ?POLL_TIMEOUT, hibernate};
	[] ->
        ?ERROR("ticket '~p' not found", [Ticket]),
		JSON = jsonify([{status, "stopped"}, {message, "error: Client not Found!"}]),
		{ok, Reply} = cowboy_req:reply(404, [], JSON, Req),
		{shutdown, Reply, #state{}}
	end.

info({ok, Packets}, Req, State = #state{callback=CB}) ->
	JSON = nextalk_json:pack(lists:reverse(Packets)),
	Reply =	replyok(CB, JSON, Req),
    {ok, Reply, State};
 
info(stop, Req, State = #state{callback=CB}) ->
	JSON = "{\"status\": \"stopped\"}",
	Reply = replyok(CB, JSON, Req),
    {ok, Reply, State};

info(Message, Req, State) ->
	?ERROR("badmsg: ~p", [Message]),
    {loop, Req, State, hibernate}.

timeout(Req, State = #state{callback=CB}) ->
	JSON = nextalk_json:pack([]),
	Reply =	replyok(CB, JSON, Req),
	{ok, Reply, State}.
 
terminate(_Reason, _Req, #state{ticket = Ticket, endpoint = Pid}) ->
	unsubscribe(Pid, Ticket),
	ok.

replyok(<<>>, JSON, Req) ->
	Headers = [{"Content-Type", "application/json"}],
	{ok, Reply} = cowboy_req:reply(200, Headers, JSON, Req),
    Reply;

replyok(CB, JSON, Req) ->
	Headers = [{"Content-Type", "application/javascript"}],
    JS = list_to_binary([CB, "(", JSON, ")"]),
	{ok, Reply} = cowboy_req:reply(200, Headers, JS, Req),
	Reply.

unsubscribe(undefined, _) ->
	ingore;
unsubscribe(Pid, Ticket)  ->
	case is_process_alive(Pid) of
	true ->
		nextalk_endpoint:unsubscribe(Pid, Ticket, self());
	false ->
		ignore
	end.

