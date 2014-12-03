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

-import(proplists, [get_value/2]).

-export([handle/2]).

-export([polling/2]).

-record(state, {callback = <<>>, ticket}).

handle(Params, Req) ->
	?INFO("Jsonp request: ~p", [Params]),
    Ticket = slim_ticket:make(get_value(<<"ticket">>, Params)),
	case slim_client:subscribe(Ticket, self()) of
	ok -> 
		Callback = proplists:get_value(<<"callback">>, Params, <<>>), 
		State = #state{callback = Callback, ticket = Ticket},
		polling(Req, State);
	{error, Code, Reason} -> 
		JSON = slim_json:encode([{status, <<"stopped">>}, {error, Reason}]),
		Req:respond({Code, [], JSON})
	end.

%%FIXME Later... this process should be hibernate...
polling(Req, #state{callback = CB, ticket = Ticket}) ->
    Response = 
	receive
		{ok, Packets} ->
            ?INFO("packets: ~p", [Packets]),
			reply(CB, slim_packet:encode(Packets), Req);
		stop ->
            ?INFO_MSG("stop..."),
			reply(CB, slim_json:encode([{status, stopped}]), Req);
		Other->
			?ERROR("polling got: ~p", [Other]),
			reply(CB, <<"{\"status\": \"ok\", \"data\":[]}">>, Req)
        after ?POLL_TIMEOUT ->  
			?ERROR("polling timeout... ~p", [Ticket]),
			reply(CB, <<"{\"status\": \"ok\", \"data\":[]}">>, Req)
	end,
    slim_client:unsubscribe(Ticket, self()),
    Response.

reply(<<>>, JSON, Req) ->
	Req:ok({"application/json", JSON});

reply(CB, JSON, Req) ->
    JS = list_to_binary([CB, "(", JSON, ")"]),
	Req:ok({"application/javascript", JS}).


