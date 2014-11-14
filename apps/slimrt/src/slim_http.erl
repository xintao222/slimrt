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

-module(slim_http).

-include("slimrt.hrl").

-include_lib("slimpp/include/slimpp.hrl").

-export([init/2,
		 terminate/3]).  

%TODO: Refactor this file later.

init(Req, Opts) ->
	case authorized(Req) of
	true ->
		{ok, Resp} = handle(Req, Opts),
		{ok, Resp, Opts};
	{false, Error} ->
		{ok, reply401(Req, Error), Opts}
	end.

authorized(Req) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"basic">>, {Domain, ApiKey}}} ->
        case slim_auth:check(Domain, ApiKey) of
        true -> true;
        {error, Error} -> {false, Error}
        end;
    Auth ->
        {false, <<"Basic realm=\"nextalk.im\"">>}
    end.

handle(Req, Opts) ->
	Method = cowboy_req:method(Req),
	Path = cowboy_req:path(Req),
	case Path of 
	<<"/", ?APIVSN, "/", ApiPath/binary>> ->
		handle(Req, Method, tokens(ApiPath), Opts).
	_ ->
		{ok, reply400(Req, <<"Bad Api Version">>)};
	end.

g(Key, Params) ->
	proplists:get_value(Key, Params).

g(Key, Params, Default) ->
	proplists:get_value(Key, Params, Default).

%TODO: write
handle(Req, <<"POST">>, {<<"online">>}, Opts) ->
	{ok, Params, Req1} = cowboy_req:body_qs(Req),
    Nick = g(nick, Params),
    {Class, Name} = nextalk_id:parse(g(name, Params)),
    Domain = g(domain, Params),
    Rooms = g(rooms, Params, <<>>),
    Buddies = g(buddies, Params, <<>>),
    Show = g(show, Params, <<"available">>),
    Status = g(status, Params, <<>>),
    UserOid = nextalk_oid:make(Class, Domain, Name),
    Rooms1 = [nextalk_oid:make(gid, Domain, Room) || Room <- bsplit(Rooms, $,)],
    Buddies1 = lists:map(fun(Buddy) -> 
		{Cls, Id} = nextalk_id:parse(Buddy),
		nextalk_oid:make(Cls, Domain, Id)
	end, bsplit(Buddies, $,)),
    {ok, CPid} =
    case nextalk_router:lookup(UserOid) of
    [] ->
        Client = #nextalk_endpoint{oid = UserOid, 
							 	name = Name, 
							 	nick = Nick, 
							 	domain = Domain, 
							 	show = b2a(Show),
							 	status = Status},
        nextalk_endpoint_sup:start_child({Client, Buddies1, Rooms1});
    [Route = #nextalk_route{pid=Pid, show = OldShow}] ->
		%在线支持好友关系问题
		nextalk_endpoint:update(Pid, {buddies, Buddies1}),
		nextalk_endpoint:update(Pid, {rooms, Rooms1}),
        %TODO: should update rooms
        if
            Show == OldShow -> ignore;
            true -> nextalk_router:update(Route#nextalk_route{show=Show})
        end,
        {ok, Route#nextalk_route.pid}
    end,
    Ticket = nextalk_ticket:make(UserOid#nextalk_oid.class, Name),
    nextalk_endpoint:bind(CPid, Ticket),
	%response
	Presences = [ {nextalk_id:from(O), Sh} || #nextalk_route{oid = O, show = Sh} 
					<- nextalk_router:lookup(Buddies1) ],
	%Totals = [ {nextalk_oid:name(Room), length(nextalk_grpchat:members(Room))} || Room <- Rooms1 ],
	Data = [{success, true},
            {ticket, nextalk_ticket:encode(Ticket)},
            {server, jsonpurl()}, 
            {jsonpd, jsonpurl()},
            {websocket, wsurl()}],
    Data1 = 
    case nextalk:isopened(mqttd) of
    true -> [{mqttd, mqtturl()} | Data];
    _ -> Data
    end,
    Data2 = 
    case Presences of
    [] -> [{presences, {}}|Data1];
    _ -> [{presences, Presences}|Data1]
    end,
	?INFO("Response: ~p", [Data2]),
	{ok, reply200(Req, Data2), State};
	{ok, Req};
handle(Req, <<"POST">>, {<<"offline">>}, Opts) ->
	{ok, Req};
handle(Req, <<"GET">>, {<<"presences">>}, Opts) ->
	{ok, Req};
handle(Req, <<"POST">>, {<<"messages">>}, Opts) ->
	{ok, Req};

handle(Req, Method, Path, Opts) ->
	{ok, reply400(Req, <<"Bad Request">>)};

terminate(_Reason, _Req, _Opts) ->
	ok.
	
qsfun(<<"GET">>) -> 
	fun(Req) -> Vals = cowboy_req:parse_qs(Req), {ok, Vals, Req} end;

qsfun(<<"POST">>) ->
	fun(Req) -> cowboy_req:body_qs(Req) end.

tokens(Path) when is_binary(Path) ->
	list_to_tuple(binary:split(Path, <<"/">>, [global])).


