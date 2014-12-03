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

%%------------------------------------------------------------------------------
%% Description:
%%
%% This module handle http api request. 
%% 
%% API list:
%%
%% endpoint online:		POST /endpoints/online
%% endpoint offline:	POST /endpoints/offline
%%
%%------------------------------------------------------------------------------

-module(slim_http).

-include_lib("slimpp/include/slimpp.hrl").

-include("slimrt.hrl").

-include("slim_log.hrl").

-import(proplists, [get_value/2, get_value/3]).

%% start http server
-export([start/1]).

%% http callback
-export([handle/1]).

%%
%% @doc start http server
%%
start(Opts) ->
	Port = get_value(port, Opts, 8080),
	case mochiweb:start_http(Port, {?MODULE, handle, []}) of
    {ok, Pid} -> {ok, Pid};
    {error,{already_started, Pid}} -> {ok, Pid}
    end.

%%
%% @doc http callback
%%
handle(Req) ->
	handle(Req:get(path), Req).

handle("/" ++ ?APIVSN ++ Path, Req) ->
	Method = Req:get(method),
	case authorized(Method, Path, Req) of
	true -> 
        Params = params(Method, Req),
        ?DEBUG("~s ~s: ~p", [Method, Path, Params]),
		handle(Method, split_path(Path), Params, Req);
	false -> 
		Req:respond({401, [], <<"Fobbiden">>})
	end;

handle(_BadPath, Req) ->
	Req:not_found().

%%------------------------------------------------------------------------------
%% HTTP API 
%%------------------------------------------------------------------------------

%%
%% Endpoint Online
%%
handle('POST', {"endpoints", "online"}, Params, Req) ->
    Domain = get_value(<<"domain">>, Params),
    RawId = get_value(<<"id">>, Params),
    {Cls, Id} = slim_id:parse(RawId),
    %endpoint oid
    EpOid = slim_oid:make(Cls, Domain, Id),
	case slim_client:online(EpOid, Params) of
	{ok, Result} ->
		ok(Result, Req);
	{error, Code, Reason} ->
		reply(Code, Reason, Req)
	end;

%%
%% Endpoint Offline
%%
handle('POST', {"endpoints", "offline"}, Params, Req) ->
    Ticket = slim_ticket:make(get_value(<<"ticket">>, Params)),
	ok = slim_client:offline(Ticket, Params),
	ok(Req);

%%
%% Publish Presence
%%
handle('POST', {"presences"}, Params, Req) ->
    Domain = get_value(<<"domain">>, Params),
    Ticket = slim_ticket:make(get_value(<<"ticket">>, Params)),
    EpOid = slim_oid:make(Domain, Ticket),
    
    %% TODO: need clientId?
    %% presence 
    Presence = #slim_presence {
        type = binary_to_atom(get_value(<<"type">>, Params), utf8),
        from = EpOid,
        nick = get_value(<<"nick">>, Params),
        priority = binary_to_integer(get_value(<<"priority">>, Params, <<"10">>)),
        show = get_value(<<"show">>, Params),
        status = get_value(<<"status">>, Params)
    },
    Presence1 = 
    case get_value(<<"to">>, Params) of
        undefined -> 
            Presence;
        ToId -> 
            {Cls, Id} = slim_id:parse(ToId),
            ToOid = slim_oid:make(Domain, Cls, Id),
            Presence#slim_presence{to = ToOid}
    end,
	case slim_client:publish(Ticket, Presence1) of
        ok -> ok(Req);
        {error, Code, Reason} -> reply(Code, Reason, Req)
    end;

%%
%% Get Presences
%%
handle('GET', {"presences"}, Params, Req) ->
    Domain = get_value(<<"domain">>, Params),
	Ids = binary:split(get_value(ids, Params), [<<",">>], [global]),
	Oids = [begin {Cls, Id} = slim_id:parse(RawId), 
				  slim_oid:make(Cls, Domain, Id) end || RawId <- Ids],
	Presences = slim_client:presences(Oids),
	ok(Presences, Req);

%% 
%% Send Message
%%
handle('POST', {"messages"}, Params, Req) ->
    Domain = get_value(<<"domain">>, Params),
	Ticket = slim_ticket:make(get_value(<<"ticket">>, Params)),
	FromOid = slim_oid:make(Domain, Ticket),
	%type
	Type = get_value(<<"type">>, Params, <<"chat">>),
	ToOid = makeoid(Domain, Type, get_value(<<"to">>, Params)),
	Message = slim_message:make(Type, FromOid, ToOid, Params),
	case slim_client:send(Ticket, Message) of
		ok -> ok(Req);
		{error, Code, Reason} -> reply(Code, Reason, Req)
	end;

%% 
%% Push Message
%%
handle('POST', {"messages", "push"}, Params, Req) ->
	Domain = get_value(<<"domain">>, Params),
	%% from oid
	{FromCls, FromId} = slim_id:parse(get_value(<<"from">>, Params)),
	FromOid = slim_oid:make(FromCls, Domain, FromId),
	%% type
	Type = get_value(<<"type">>, Params, <<"chat">>),
	%% to oid
	ToOid = makeoid(Domain, Type, get_value(<<"to">>, Params)),
	Message = slim_message:make(Type, FromOid, ToOid, Params),
	ok = slim_client:push(FromOid, ToOid, Message),
    ok(Req);

%% 
%% Join Room
%%
handle('POST', {"rooms", RoomId, "join"}, Params, Req) ->
	RoomOid = slim_oid:make(gid, get_value(<<"domain">>, Params), RoomId),
	case slim_client:join_room(ticket(Params), RoomOid, Params) of
	ok -> ok(Req);
	{error, Code, Reason} -> reply(Code, Reason, Req)
	end;

%% 
%% Leave Room
%%
handle('POST', {"rooms", RoomId, "leave"}, Params, Req) ->
	RoomOid = slim_oid:make(gid, get_value(<<"domain">>, Params), RoomId),
	case slim_client:leave_room(ticket(Params), RoomOid, Params) of
	ok -> ok(Req);
	{error, Code, Reason} -> reply(Code, Reason, Req)
	end;

%% 
%% Get presences of room members
%%
handle('GET', {"rooms", RoomId, "members"}, Params, Req) ->
	RoomOid = slim_oid:make(gid, get_value(<<"domain">>, Params), RoomId),
	case slim_client:room_members(ticket(Params), RoomOid) of
	{ok, Members} -> ok(Members, Req);
	{error, Code, Reason} -> reply(Code, Reason, Req)
	end;

%%------------------------------------------------------------------------------
%% Long Polling
%%------------------------------------------------------------------------------
handle('GET', {"packets"}, Params, Req) ->
	slim_jsonp:handle(Params, Req);

%%------------------------------------------------------------------------------
%% Websocket connection
%%------------------------------------------------------------------------------
handle(_Method, _Path, _Params, Req) ->
	Req:not_found().

%%------------------------------------------------------------------------------
%% HTTP Basic Auth
%%------------------------------------------------------------------------------
authorized('GET', "/packets", _Req) ->
	true; %%no need to auth

authorized(_, _Path, Req) ->
	case mochiweb_request:get_header_value("Authorization", Req) of
	undefined -> false;
	"Basic " ++ BasicAuth ->
		[Domain, ApiKey] = binary:split(base64:decode(BasicAuth), <<":">>),
        case slim_auth:check(Domain, ApiKey) of
        true -> true;
        {error, _Error} -> false
        end
	end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
ticket(Params) ->
	slim_ticket:make(get_value(<<"ticket">>, Params)).

makeoid(Domain, Type, RawId) ->
	{Cls, Id} = slim_id:parse(RawId),
	case Type of
	<<"chat">> -> 
		slim_oid:make(Cls, Domain, Id);
	<<"grpchat">> -> 
		slim_oid:make(gid, Domain, Id)
	end.

split_path(Path) ->
	list_to_tuple(string:tokens(Path, "/")).

params('GET', Req) ->
	[{list_to_binary(Key), list_to_binary(Value)} 
		|| {Key, Value} <- mochiweb_request:parse_qs(Req)];

params('POST', Req) ->
	[{list_to_binary(Key), list_to_binary(Value)} 
		|| {Key, Value} <- mochiweb_request:parse_post(Req)].

ok(Req) ->
	Json = slim_json:encode([{status, ok}]),
	Req:ok({"application/json", Json}).

ok(Req, Data) ->
	Json = slim_json:encode([{status, ok}, {data, Data}]),
	Req:ok({"application/json", Json}).

reply(Code, Req) when (code >= 200) and (Code < 300)  ->
	Json = slim_json:encode([{status, ok}]),
	Req:respond({Code, [{"Content-Type", "text/plain"}], Json}). 
	
reply(Code, Data, Req) when (code >= 200) and (Code < 300) ->
	Json = slim_json:encode([{status, ok}, {data, Data}]),
	Req:respond({Code, [{"Content-Type", "text/plain"}], Json});

reply(Code, Reason, Req) when (code >= 400) ->
	Json = slim_json:encode([{status, error}, {reason, Reason}]),
	Req:respond({Code, [{"Content-Type", "text/plain"}], Json}). 


