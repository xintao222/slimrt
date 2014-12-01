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

-export([handle/2]).

-export([polling/2]).

-record(state, {callback = <<>>, ticket, endpoint, timer}).

handle(Params, Req) ->
	?INFO("Jsonp request: ~p", [Params]),
    Ticket = slim_ticket:make(proplists:get_value(<<"ticket">>, Params)),
	case slim_cm:lookup(Ticket) of
	Pid when is_pid(Pid) ->
		slim_client:subscribe(Pid, Ticket),
		Callback = proplists:get_value(<<"callback">>, Params, <<>>), 
		Ref = erlang:send_after(?POLL_TIMEOUT, self(), timeout),
		State = #state{callback = Callback, ticket = Ticket, endpoint = Pid, timer = Ref},
		proc_lib:hibernate(?MODULE, polling, [Req, State]);
	undefined -> 
		JSON = slim_json:encode([{status, <<"stopped">>}, {error, <<"Ticket not bound">>}]),
		Req:respond({404, [], JSON})
	end.

polling(Req, #state{callback = CB, ticket = Ticket, endpoint = Pid, timer = Ref}) ->
	?INFO_MSG("polling......"),
	erlang:cancle_timer(Ref),
	slim_client:unsubscribe(Pid, Ticket),
	receive
		{ok, Packets} ->
			JSON = slim_packet:encode(Packets),
			reply(CB, JSON, Req);
		stop ->
			JSON = slim_json:encode([{status, stopped}]),
			reply(CB, JSON, Req);
		timeout ->
			?ERROR("polling timeout... ~p", [Ticket]),
			reply(CB, <<"{\"status\": \"ok\", \"data\":[]}">>, Req);
		Other->
			?ERROR("polling got: ~p", [Other]),
			reply(CB, <<"{\"status\": \"ok\", \"data\":[]}">>, Req)
	end.

reply(<<>>, JSON, Req) ->
	Headers = [{"Content-Type", "application/json"}],
	Req:respond({200, Headers, JSON});

reply(CB, JSON, Req) ->
	Headers = [{"Content-Type", "application/javascript"}],
    JS = list_to_binary([CB, "(", JSON, ")"]),
	Req:respond({200, Headers, JS}).

