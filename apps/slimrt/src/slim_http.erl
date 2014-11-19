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

-import(proplists, [get_value/2, get_value/3]).

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
	<<"/", ?APIVSN, ApiPath/binary>> ->
		handle(Req, Method, ApiPath, Opts).
	_ ->
		{ok, reply400(Req, <<"Bad Api Path">>)};
	end.

handle(Req, <<"POST">>, <<"/presences/online">>, Opts) ->
	{ok, Params, Req1} = cowboy_req:body_qs(Req),
    Domain = get_value(<<"domain">>, Params),
    {Class, Name} = slim_id:parse(get_value(<<"name">>, Params)),
    UserOid = slim_oid:make(Class, Domain, Name),
	%FIXME
	case slim_client:online(UserOid,  Params) of
	{ok, Data} -> 
		{ok, reply(Req1, 200, Data)};
	{error, Code, Reason} ->
		{ok, reply(Req1, Code, Reason)}
	end;

handle(Req, <<"POST">>, <<"/presences/offline">>, Opts) ->
	{ok, Params, Req1} = cowboy_req:body_qs(Req),
    Ticket = slim_ticket:make(get_value(<<"ticket">>, Params)),
	slim_client:offline(Ticket, Params);
	{ok, reply(Req1, 200, [{status, ok}]}.

handle(Req, <<"POST">>, <<"/presences/show">>, Opts) ->
	{ok, Req};

handle(Req, <<"GET">>, <<"/presences">>, Opts) ->
    Domain = g(domain, Params),
	Presences = slim_client:get_presences(Domain, Params),
	{ok, reply(Req, 200, [{data, Presences}])};
	

handle(Req, <<"POST">>, <<"/messages/send">>, Opts) ->
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

handle(Req, <<"POST">>, <<"/messages/push">>, Opts) ->
	{ok, Req};

handle(Req, <<"POST">>, <<"/rooms/join">>, Opts) ->
	{ok, Req};
handle(Req, <<"POST">>, <<"/rooms/leave">>, Opts) ->
	{ok, Req};
handle(Req, <<"POST">>, <<"/rooms/members">>, Opts) ->
	{ok, Req};

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

tokens(Path) when is_binary(Path) ->
	list_to_tuple(binary:split(Path, <<"/">>, [global])).


