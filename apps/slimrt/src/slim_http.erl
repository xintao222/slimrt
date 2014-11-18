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
        {false, <<"Basic realm=\"", Domain, "\"">>}
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


%TODO: write
handle(Req, <<"POST">>, {<<"online">>}, Opts) ->
	{ok, Params, Req1} = cowboy_req:body_qs(Req),
    Nick = g(<<"nick">>, Params),
    {Class, Name} = slim_id:parse(g(<<"name">>, Params)),
    Domain = g(<<"domain">>, Params),
    Rooms = g(<<"rooms">>, Params, <<>>),
    Buddies = g(<<"buddies">>, Params, <<>>),
    Show = g(<<"show">>, Params, <<"available">>),
    Status = g<<"(status">>, Params, <<>>),
    UserOid = slim_oid:make(Class, Domain, Name),
    Rooms1 = [slim_oid:make(gid, Domain, Room) || Room <- bsplit(Rooms, $,)],
    Buddies1 = lists:map(fun(Buddy) -> 
		{Cls, Id} = slim_id:parse(Buddy),
		slim_oid:make(Cls, Domain, Id)
	end, bsplit(Buddies, $,)),
    {ok, CPid} =
    case slim_router:lookup(UserOid) of
    [] ->
        Endpoint = #slim_endpoint{oid = UserOid, 
							 	name = Name, 
							 	nick = Nick, 
							 	domain = Domain, 
							 	show = binary_to_atom(Show),
							 	status = Status},
        slim_endpoint_sup:start_child({Endpoint, Buddies1, Rooms1});
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
	%Totals = [ {slim_oid:name(Room), length(slim_grpchat:members(Room))} || Room <- Rooms1 ],
	//TODO: FIX SERVER
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
	{ok, reply200(Req, Data2), State};
	{ok, Req};
handle(Req, <<"POST">>, {<<"offline">>}, Opts) ->
    Domain = g(<<"domain">>, Params),
    Ticket = slim_ticket:make(g(<<"ticket">>, Params)),
	UserOid = makeoid(Domain, Ticket),
	case slim_router:lookup(UserOid) of
	[Route] ->
		slim_endpoint:unbind(Route#slim_route.pid, Ticket);
	[] ->
		?ERROR("~s alread offline...", [slim_oid:topic(UserOid)])
	end,
	{ok, replyok(Req)};

%%TODO: fix 
handle(Req, <<"GET">>, {<<"presences">>}, Opts) ->
    Domain = g(domain, Params),
	Ids = binary:split(g(ids, Params), [<<",">>], [global]),
	Oids = [begin {Cls, Id} = slim_id:parse(RawId), 
				  slim_oid:make(Cls, Domain, Id) 
		    end || RawId <- Ids],
	Response = [ {slim_id:from(O), Show} 
				 || #slim_route{oid = O, show = Show} 
				 <- slim_router:lookup(Oids) ],
	{ok, reply200(Req, Response)};

handle(Req, <<"POST">>, {<<"messages">>}, Opts) ->
	{ok, Params, Req1} = cowboy_req:body_qs(Req),	

	%Domain
    Domain = g(<<"domain">>, Params),

	//TODO:......

	%FromOid
	{Ticket, FromOid} =
	case g(<<"ticket">>, Params) of
	undefined -> %Push Message
		{FromCls, From} = slim_id:parse(g(<<"from">>, Params)),
		{undefined, slim_oid:make(FromCls, Domain, From)};
	S -> %Send Message
		T = slim_ticket:make(S),
		{T, makeoid(Domain, T)}
	end,

	%ToOid
	Type = binary_to_atom(g(<<"type">>, Params, <<"chat">>)),
	{ToCls, To} = slim_id:parse(g(<<"to">>, Params)),
	ToOid = 
	case Type of
	chat -> 
		slim_oid:make(ToCls, Domain, To);
	grpchat -> 
		slim_oid:make(gid, Domain, To)
	end,
	
	Message = slim_message:make(Type, FromOid, ToOid, Params),
	case slim_router:lookup(UserOid) of
	[#slim_route{pid=CPid}] ->
		%send message
		slim_endpoint:send(CPid, Ticket, ToOid, Message);
	[] ->
		%publish directly
		slim_router:route(UserOid, ToOid, Message)
	end,
	{ok, replyok(Req1)};

handle(Req, Method, Path, Opts) ->
	{ok, reply400(Req, <<"Bad Request">>)};

terminate(_Reason, _Req, _Opts) ->
	ok.

replyok(Req) ->
	{ok, Reply} = cowboy_req:reply(200, [], jsonify_ok(), Req), 
	Reply.

reply200(Req, Term) ->
	{ok, Reply} = cowboy_req:reply(200, [{"Content-Type", "text/plain"}], jsonify(Term), Req), 
	Reply.
	
reply400(Req, Msg) ->
	{ok, Reply} = cowboy_req:reply(400, [], jsonify_error(Msg), Req), 
	Reply.

reply401(Req, Msg) ->
	{ok, Reply} = cowboy_req:reply(401, [], jsonify_error(Msg), Req), 
	Reply.

makeoid(Domain, #slim_ticket{class=Cls, name=Name}) ->
	slim_oid:make(Cls, Domain, Name).

jsonify_ok() ->
	jsonify([{status, ok}]).

jsonify_error(Msg) when is_list(Msg) ->
    jsonify_error(list_to_binary(Msg));

jsonify_error(Msg) when is_binary(Msg) ->
	jsonify([{status, error}, {message, Msg}]).

g(Key, Params) ->
	proplists:get_value(Key, Params).

g(Key, Params, Default) ->
	proplists:get_value(Key, Params, Default).

tokens(Path) when is_binary(Path) ->
	list_to_tuple(binary:split(Path, <<"/">>, [global])).




