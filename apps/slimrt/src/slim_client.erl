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

-module(slim_client).

-author('feng.lee@slimchat.io').

-include_lib("slimpp/include/slimpp.hrl").

-include("slimrt.hrl").

-include("slim_log.hrl").

-import(proplists, [get_value/2, get_value/3]).

-export([online/2, offline/2]).

-export([subscribe/2, unsubscribe/2]).

-spec online(EpOid :: oid(), Params :: list(tuple())) -> {ok, any()} | {error, integer(), binary()}.
online(EpOid, Params) when ?is_oid(EpOid) ->
	%domain
    Domain = get_value(<<"domain">>, Params),

	%clientId
	ClientId = get_value(<<"clientId">>, Params),

	%properties
    Name = get_value(<<"name">>, Params),
    Nick = get_value(<<"nick">>, Params),
    Show = get_value(<<"show">>, Params, <<"available">>),
    Status = get_value(<<"status">>, Params, <<>>),

	%%buddies
    Buddies = parse(buddies, Domain, Params),
	Rooms = parse(rooms, Domain, Params),

	%% find or create endpoint
    {ok, EPid} =
    case slim_router:lookup(EpOid) of
    [] ->
		Endpoint = #slim_endpoint{
				oid = EpOid, 
				name = Name, 
				nick = Nick, 
				show = Show,
				status = Status},
        slim_endpoint_sup:start_child({Endpoint, Buddies, Rooms});
    [#slim_route{pid=Pid}] ->
		%在线支持好友关系问题 %%FIXME: 不合理.... clientId优先级....should have a roster version tag...
		slimp_endpoint:update(Pid, [{buddies, Buddies}, {rooms, Rooms}, {show, Show}]),
        {ok, Pid}
    end,
	%%TODO: bind the clientId...return ticket and all clients??
	{ok, Ticket, Clients} = slim_endpoint:bind(EPid, ClientId),
	Response = [{ticket, slim_ticket:encode(Ticket)},
                {server, slim_port:addrs()},
				{clients, Clients},
				{presences, presences(Buddies)}],
	{ok, Response}.

-spec offline(Ticket :: ticket(), Params :: list(tuple())) -> ok.
offline(Ticket, Params) ->
	case slim_climgr:lookup(Ticket) of
	undefined ->
		?ERROR("~s is alread offline...", [Ticket]);
	EPid ->
		slim_endpoint:unbind(EPid, Ticket)
	end.

-spec presences(Oids :: list(oid())) -> list(tuple()).
presences(Oids) when is_list(Oids) ->
	[{slim_id:from(Oid), Show} || #slim_route{oid = Oid, show = Show} <- slim_router:lookup(Oids)].


publish(Ticket, Presence) when ?is_ticket(Ticket), ?is_presence(Presence) ->
	%%TOOD: 
	ok.
	
send(Ticket, Message) when ?is_ticket(Ticket), ?is_message(Message) ->
	case slim_cm:lookup(Ticket) of
	EPid when is_pid(EPid) ->
		slim_endpoint:send(EPid, Ticket, Message);
	undefined -> 
		{error, 500, "Client Not Found"}
	end.

push(FromOid, ToOid, Message) when ?id_oid(FromOid), ?is_message(Message) ->
	%publish directly
	slim_router:route(FromOid, ToOid, Message).

%%TODO: subscribe(Ticket, Pid)
subscribe(Endpoint, Ticket) when is_pid(Endpoint) and ?is_ticket(Ticket) ->
	slim_endpoint:subscribe(Endpoint, Ticket, self()).

%%TODO: unsubscribe(Ticket, Pid)
unsubscribe(Endpoint, Ticket) ->
	slim_endpoint:unsubscribe(Endpoint, Ticket, self()).

join(Ticket, RoomOid) when  ->
%%TODO: join room...
    ok.

leave(Ticket, RoomOid) ->
%%TODO: leave room...
    ok.

members(Ticket, RoomOid) ->
%%TODO: get room members...
    ok.

%%---------------------------------------------------
%% internal 
%%---------------------------------------------------
bsplit(Bin, Sep) ->
	binary:split(Bin, [Sep], [global]).

parse(buddies, Domain, Params) ->
	Input = get_value(<<"buddies">>, Params, <<>>),
    lists:map(fun(I) -> 
		{Tag, Id} = slim_id:parse(I),
		slim_oid:make(Tag, Domain, Id)
	end, bsplit(Input, <<",">>));

parse(rooms, Domain, Params) ->
    Rooms = get_value(<<"rooms">>, Params, <<>>),
    [slim_oid:make(gid, Domain, Room) 
		|| Room <- bsplit(Rooms, <<",">>)].

makeoid(Domain, #slim_ticket{class=Cls, name=Name}) ->
	slim_oid:make(Cls, Domain, Name).

makeoid(Domain, Type, Name) ->
	%%FIXME:......
	{Tag, Id} = slim_id:parse(Name),
	case Type of
	<<"chat">> -> 
		slim_oid:make(Tag, Domain, Id);
	<<"grpchat">> -> 
		slim_oid:make(gid, Domain, Id)
	end.

