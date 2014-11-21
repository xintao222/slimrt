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

-include_lib("slimpp/include/slimpp.hrl").

-include("slimrt.hrl").

-include("slim_log.hrl").

-import(lists, [reverse/1]).

-import(proplists, [get_value/2, get_value/3]).

-export([init/2, info/3, terminate/3]).

-record(state, {callback = <<>>, ticket, endpoint}).

init(Req, Opts) ->
	?INFO("Json handler opts: ~p", [Opts]),
	{ok, Params, Req1} = cowboy_req:qs_vals(Req),
	?INFO("Jsonp request: ~p", [Params]),
    Ticket = slim_ticket:make(get_value(<<"ticket">>, Params)),
	case slim_cm:lookup(Ticket) of
	Pid when is_pid(Pid) ->
		slim_client:subscribe(Pid, Ticket),
		Callback = get_value(<<"callback">>, Params, <<>>), 
		State = #state{callback = Callback, ticket = Ticket, endpoint = Pid},
		{cowboy_loop, Req1, State, ?POLL_TIMEOUT, hibernate};
	undefined -> 
		JSON = slim_json:encode([{status, <<"stopped">>}, {error, "Ticket not bound"}]),
		{ok, Reply} = cowboy_req:reply(404, [], JSON, Req1),
		{shutdown, Reply, #state{}}
	end.

info({ok, Packets}, Req, State = #state{callback=CB}) ->
	JSON = slim_packet:encode(Packets),
    {ok, reply(CB, JSON, Req), State};
 
info(stop, Req, State = #state{callback=CB}) ->
	JSON = slim_json:encode([{status, stopped}]),
    {ok, reply(CB, JSON, Req), State};

info(Message, Req, State) ->
	?ERROR("badmsg: ~p", [Message]),
    {loop, Req, State, hibernate}.

%%FIXME LATER
%%timeout(Req, State = #state{callback=CB}) ->
%%	JSON = slim_packet:encode([]),
%%	Reply =	replyok(CB, JSON, Req),
%%	{ok, Reply, State}.
%%
 
terminate(_Reason, _Req, #state{endpoint = undefined}) ->
	ok;
terminate(_Reason, _Req, #state{ticket = Ticket, endpoint = Pid}) ->
	slim_client:unsubscribe(Pid, Ticket). 

reply(<<>>, JSON, Req) ->
	Headers = [{"Content-Type", "application/json"}],
	cowboy_req:reply(200, Headers, JSON, Req);

reply(CB, JSON, Req) ->
	Headers = [{"Content-Type", "application/javascript"}],
    JS = list_to_binary([CB, "(", JSON, ")"]),
	cowboy_req:reply(200, Headers, JS, Req).

