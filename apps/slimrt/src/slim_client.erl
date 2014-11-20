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

input(<<"buddies">>, Domain, Params) ->
	Input = get_value(<<"buddies">>, Params, <<>>),
    lists:map(fun(I) -> 
		{Tag, Id} = slim_id:parse(I),
		slim_oid:make(Tag, Domain, Id)
	end, bsplit(Input, <<",">>));

input(<<"rooms">>, Domain, Params) ->
    Rooms = get_value(<<"rooms">>, Params, <<>>),
    [slim_oid:make(gid, Domain, Room) 
		|| Room <- bsplit(Rooms, <<",">>)];

input(Key, Params) when is_binary(Key) ->
	get_value(Key, Params).

online(FromOid, Params) ->
	%domain
    Domain = get_value(<<"domain">>, Params),

	%properties
    Nick = get_value(<<"nick">>, Params),
    Show = get_value(<<"show">>, Params, <<"available">>),
    Status = get_value(<<"status">>, Params, <<>>),

	%%buddies
    Buddies = input(<<"buddies">>, Params),
	Rooms = input(<<"rooms">>, Params),

    {ok, CPid} =
    case slim_router:lookup(UserOid) of
    [] ->
        slim_endpoint_sup:start_child({FromOid, Buddies, Rooms});
		%%FIXME Later: slim_endpoint:send(#slim_presence{});
    [Route = #slim_route{pid=Pid, show = OldShow}] ->
		%在线支持好友关系问题
		slim_endpoint:update(Pid, {buddies, Buddies1}),
		slim_endpoint:update(Pid, {rooms, Rooms1}),
        %TODO: should update rooms
        if
            Show == OldShow -> ignore;
            true -> slim_router:update(Route#slim_route{show=Show})
        end,
        {ok, Route#slim_route.pid}
    end,

    Ticket = slim_ticket:make(UserOid#slim_oid.class, Name),
    slim_endpoint:bind(CPid, Ticket),
	%response
	Presences = [ {slim_id:from(O), Sh} || #slim_route{oid = O, show = Sh} 
					<- slim_router:lookup(Buddies1) ],
	%TODO: FIX SERVER
	Data = [{success, true},
            {ticket, slim_ticket:encode(Ticket)},
            {server, [{jsonpd, slim_port:addr(jsonp)}, {websocket, slim_port:addr(wsocket)}]}
		   ],
    Data1 = 
    case slimrt_port:isopened(mqttd) of
    true -> [{mqttd, slim_port:addr(mqtt)} | Data];
    _ -> Data
    end,
    Data2 = 
    case Presences of
    [] -> [{presences, {}}|Data1];
    _ -> [{presences, Presences}|Data1]
    end,
	?INFO("Response: ~p", [Data2]),
	{ok, Data2}.

offline(Ticket, Params) ->
	case slim_climgr:lookup(Ticket) of
	undefined ->
		?ERROR("~s is alread offline...", [Ticket]);
	Pid ->
		slim_endpoint:unbind(Pid, Ticket)
	end.

%%TODO: should be fixed...
get_presences(Domain, Ids) when is_list(Ids) ->
	Oids = [begin {Cls, Id} = slim_id:parse(RawId), 
				  slim_oid:make(Cls, Domain, Id) 
		    end || RawId <- Ids],
	[ {slim_id:from(O), Show} || 
		#slim_route{oid = O, show = Show} <- slim_router:lookup(Oids) ].

set_presence(Params) ->
    ok.

send_message(Ticket, Params) ->
	%from
    Domain = get_value(<<"domain">>, Params),
	FromOid = makeoid(Domain, Ticket),
	
	%type
	Type = get_value(<<"type">>, Params, <<"chat">>),

	%to
	{ToCls, To} = slim_id:parse(get_value(<<"to">>, Params)),
	ToOid = 
	case Type of
	<<"chat">> -> 
		slim_oid:make(ToCls, Domain, To);
	<<"grpchat">> -> 
		slim_oid:make(gid, Domain, To)
	end,
	Message = slim_message:make(Type, FromOid, ToOid, Params),
	case slim_cm:lookup(Ticket) of
	Pid when is_pid(Pid) ->
		slim_endpoint:send(Pid, Ticket, ToOid, Message);
	undefined -> 
		{error, "Client Not Found"}
	end.

makeoid(Domain, Type, Name) ->
	%%FIXME:......
	{Tag, To} = slim_id:parse(get_value(<<"to">>, Params)),
	case Type of
	<<"chat">> -> 
		slim_oid:make(Tag, Domain, To);
	<<"grpchat">> -> 
		slim_oid:make(gid, Domain, To)
	end.

push_message(FromOid, Params) ->
	%%FIXME:......
	Domain = get_value(<<"domain">>, Params),
	Type = get_value(<<"type">>, Params, <<"chat">>),
	To = get_value(<<"to">>, Params), 
	ToOid = makeoid(Domain, Type, To),
	Message = slim_message:make(Type, FromOid, ToOid, Params),
	%publish directly
	slim_router:route(FromOid, ToOid, Message).

join_room() ->
    ok.

leave_room() ->
    ok.

room_members() ->
    ok.

subscribe(Endpoint, Ticket) when is_pid(Endpoint) and ?is_ticket(Ticket) ->
	slim_endpoint:subscribe(Endpoint, Ticket, self()).

unsubscribe(Endpoint, Ticket) ->
	slim_endpoint:unsubscribe(Endpoint, Ticket, self()).

bsplit(Bin, Sep) ->
	binary:split(Bin, [Sep], [global]).


