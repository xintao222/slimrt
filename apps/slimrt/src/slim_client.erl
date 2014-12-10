%%-----------------------------------------------------------------------------
%% Copyright (c) 2014, Feng Lee <feng@slimchat.io>
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

-author('feng@slimchat.io').

-include_lib("slimpp/include/slimpp.hrl").

-include("slimrt.hrl").

-include("slim_log.hrl").

-import(proplists, [get_value/2, get_value/3]).

%% endpoints api
-export([online/2, offline/2]).

%% presence api
-export([presences/1, publish/2]).

%% messages api
-export([push/3, send/2]).

%% subscribe, unsubscribe
-export([subscribe/2, unsubscribe/2]).

%% rooms api
-export([join_room/3, leave_room/3, room_members/2]).

%%
%% @doc endpoint online...
%%
-spec online(EpOid :: oid(), Params :: list(tuple())) -> {ok, any()} | {error, integer(), binary()}.
online(EpOid, Params) when ?is_oid(EpOid) ->
	%domain
    Domain = get_value(<<"domain">>, Params),

	%TODO: clientId, will be supported 0.2 version...
	%%ClientId = get_value(<<"clientId">>, Params),

	%properties
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
				nick = Nick, 
				show = Show,
				status = Status},
        slim_endpoint_sup:start_child({Endpoint, Buddies, Rooms});
    [#slim_route{pid=Pid}] ->
		%在线支持好友关系问题 %%FIXME: 不合理.... clientId优先级....should have a roster version tag...
		slimp_endpoint:update(Pid, [{buddies, Buddies}, {rooms, Rooms}, {show, Show}]),
        {ok, Pid}
    end,
	%%TODO: bind the clientId...return ticket and all clients?? return clients???
	{ok, Ticket} = slim_endpoint:bind(EPid, <<"ClientId">>), %TODO: 0.2 version support clientId, Clients
	Response = [{ticket, slim_ticket:encode(Ticket)},
                {server, slim_port:addrs()},
				%{clients, Clients},
				{presences, presences(Buddies)}],
	{ok, Response}.

%%
%% @doc endpoint offline...
%%
-spec offline(Ticket :: ticket(), Params :: list(tuple())) -> ok.
offline(Ticket, _Params) ->
	case slim_cm:lookup(Ticket) of
	EPid when is_pid(EPid) ->
		slim_endpoint:unbind(EPid, Ticket);
	undefined ->
		?ERROR("~s is alread offline...", [Ticket])
	end.

%%
%% Lookup presences
%%
-spec presences(Oids :: list(oid())) -> list(tuple()).
presences(Oids) when is_list(Oids) ->
	[{slim_id:from(Oid), Show} || #slim_route{oid = Oid, show = Show} <- slim_router:lookup(Oids)].


%%
%% @doc Publish Presence
%%
publish(Ticket, Presence) when ?is_ticket(Ticket), ?is_presence(Presence) ->
	case slim_cm:lookup(Ticket) of
	EPid when is_pid(EPid) ->
		ok = slim_endpoint:publish(EPid, Ticket, Presence);
	undefined ->
		{error, 500, <<"Ticket Not Found.">>}
	end.
	
%%
%% @doc Send Message
%%
send(Ticket, Message) when ?is_ticket(Ticket), ?is_message(Message) ->
	case slim_cm:lookup(Ticket) of
	EPid when is_pid(EPid) ->
		ok = slim_endpoint:send(EPid, Ticket, Message);
	undefined -> 
		{error, 500, <<"Ticket Not Found.">>}
	end.

%%
%% @doc Push Message Directly
%%
push(FromOid, ToOid, Message) when ?is_oid(FromOid), ?is_oid(ToOid), ?is_message(Message) ->
	%publish directly???
	ok = slim_router:route(FromOid, ToOid, Message).

%%
%% @doc subscribe endpoint to receive packets
%%
-spec subscribe(Ticket :: ticket(), SubPid :: pid()) -> ok | {error, integer(), binary()}.
subscribe(Ticket, SubPid) when ?is_ticket(Ticket), is_pid(SubPid) ->
	case slim_cm:lookup(Ticket) of
	EPid when is_pid(EPid) ->
		ok = slim_endpoint:subscribe(EPid, Ticket, SubPid);
	undefined  ->
		{error, 500, <<"Ticket Not Found">>}
	end.
%%
%% @doc unsubscribe endpoint
%%
-spec unsubscribe(Ticket :: ticket(), SubPid :: pid()) -> ok.
unsubscribe(Ticket, SubPid) when ?is_ticket(Ticket), is_pid(SubPid) ->
	case slim_cm:lookup(Ticket) of
	EPid when is_pid(EPid) ->
		ok = slim_endpoint:unsubscribe(EPid, Ticket, SubPid);
	undefined ->
		?ERROR("cannot unsubscribe: ~p", [Ticket]),
		{error, 500, <<"Ticket Not Found">>}
	end.

%%------------------------------------------------------------------------------
%% Room API: join, leave, members
%%------------------------------------------------------------------------------
join_room(Ticket, RoomOid, Params) when ?is_ticket(Ticket), ?is_oid(RoomOid) ->
	case slim_cm:lookup(Ticket) of
	EPid when is_pid(EPid) ->
		Nick = get_value(<<"nick">>, Params, Params),
		ok = slim_endpoint:join(EPid, RoomOid, Nick);
	undefined ->
		{error, 500, <<"Ticket Not Found">>}
	end.

leave_room(Ticket, RoomOid, Params) when ?is_ticket(Ticket), ?is_oid(RoomOid) ->
	case slim_cm:lookup(Ticket) of
	EPid when is_pid(EPid) ->
		Nick = get_value(<<"nick">>, Params),
		ok = slim_endpoint:leave(EPid, RoomOid, Nick);
	undefined ->
		{error, 500, <<"Ticket Not Found">>}
	end.

room_members(Ticket, RoomOid) when ?is_ticket(Ticket), ?is_oid(RoomOid) ->
	case slim_cm:lookup(Ticket) of
	EPid when is_pid(EPid) ->
		Oids = [Oid || #slim_room{oid = Oid} <- slim_grpchat:members(RoomOid)],
        {ok, [{slim_id:from(O), Show} 
			 	|| #slim_route{oid = O, show = Show} 
					<- slim_router:lookup(Oids)]};
	undefined ->
		{error, 500, <<"Ticket Not Found">>}
	end.
	
%%---------------------------------------------------
%% internal 
%%---------------------------------------------------
bsplit(Bin, Sep) ->
	binary:split(Bin, [Sep], [global]).

parse(buddies, Domain, Params) ->
	Input = get_value(<<"buddies">>, Params, <<>>),
    lists:map(fun(I) -> 
		{Cls, Id} = slim_id:parse(I),
		slim_oid:make(Cls, Domain, Id)
	end, bsplit(Input, <<",">>));

parse(rooms, Domain, Params) ->
    Rooms = get_value(<<"rooms">>, Params, <<>>),
    [slim_oid:make(gid, Domain, Room) 
		|| Room <- bsplit(Rooms, <<",">>)].
