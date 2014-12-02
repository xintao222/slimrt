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
%% This module handle http api request. Api list:
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
	handle(Req:get(path), Req);

handle("/" ++ ?APIVSN ++ Path, Req) ->
	Method = Req:get(method),
	case authorized(Method, Path, Req) of
	true -> 
		handle(Method, split_path(Path), Req);
	false -> 
		Req:respond({401, [], "Fobbiden"})
	end;

handle(_BadPath, Req) ->
	Req:not_found().

%%------------------------------------------------------------------------------
%% HTTP API 
%%------------------------------------------------------------------------------

%%
%% Endpoint Online
%%
handle('POST', {"endpoints", "online"}, Req) ->
	Params = params(post, Req),
	?INFO("endpoints online: ~p", [Params]),
    Domain = get_value(<<"domain">>, Params),
    {Class, Name} = slim_id:parse(get_value(<<"name">>, Params)),
    EpOid = slim_oid:make(Class, Domain, Name),
	case slim_client:online(EpOid, Params) of
	{ok, Result} -> 
		reply(200, Result, Req);
	{error, Code, Reason} ->
		reply(Code, Reason, Req)
	end;

%%
%% Endpoint Offline
%%
handle('POST', {"endpoints", "offline"}, Req) ->
	Params = params(post, Req),
	?INFO("endpoints offline: ~p", [Params]),
    Ticket = slim_ticket:make(get_value(<<"ticket">>, Params)),
	slim_client:offline(Ticket, Params),
	reply(200, Req);

%%
%% Publish Presence
%%
handle('POST', {"presences"}, Req) ->
	%% update presences??
	%%TODO: NEED priority, check xmpp protocol????
	slim_client:publish(Ticket, Presence),
	%TODO: ....
	reply(200, Req);

%%
%% Get Presences
%%
handle('GET', {"presences"}, Req) ->
	Params = params(get, Req),
    Domain = get_value(<<"domain">>, Params),
	Ids = binary:split(get_value(ids, Params), [<<",">>], [global]),
	Oids = [begin {Cls, Id} = slim_id:parse(RawId), 
				  slim_oid:make(Cls, Domain, Id) end || RawId <- Ids],
	Presences = slim_client:presences(Oids),
	reply(200, Presences, Req);

%% 
%% Send Message
%%
handle('POST', {"messages"}, Req) ->
	Params = params(post, Req),
	Ticket = slim_ticket:make(get_value(<<"ticket">>, Params)),
	case slim_client:send_message(Ticket, Params) of
	ok ->
		reply(200, Req);
	{error, Reason} ->
		reply(500, Reason, Req)
	end;

%% 
%% Push Message
%%
handle('POST', {"/messages", "push"}, Req) ->
	Params = params(post, Req),
	Domain = get_value(<<"domain">>, Params),
	{FromCls, From} = slim_id:parse(get_value(<<"from">>, Params)),
	FromOid = slim_oid:make(FromCls, Domain, From),
	case slim_client:push_message(FromOid, Params) of
	ok ->
		reply(200, Req);
	{error, Reason} ->
		reply(500, Reason, Req)
	end;

%% 
%% Join Room
%%
handle('POST', {"rooms", RoomId, "join"}, Req) ->
	reply(200, Req);

%% 
%% Leave Room
%%
handle('POST', {"rooms", RoomId, "leave"}, Req) ->
	reply(200, Req);

%% 
%% Get presences of room members
%%
handle('GET', {"rooms", RoomId, "members"}, Req) ->
	reply(200, Req);


%%------------------------------------------------------------------------------
%% Long Polling
%%------------------------------------------------------------------------------
handle('GET', {"packets"}, Req) ->
	slim_jsonp:handle(params(get, Req), Req);

handle(_Method, _Path, Req) ->
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
split_path(Path) ->
	list_to_tuple(string:tokens(Path, "/")).

params(get, Req) ->
	[{list_to_binary(Key), list_to_binary(Value)} 
		|| {Key, Value} <- mochiweb_request:parse_qs(Req)];

params(post, Req) ->
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


