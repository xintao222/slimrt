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

-include_lib("slimpp/include/slimpp.hrl").

-include("slimrt.hrl").

-include("slim_api.hrl").

-include("slim_log.hrl").

-import(proplists, [get_value/2, get_value/3]).

-export([start/1]).

%%callback....
-export([handle/1]).

%%
%% @doc start http server
%%
start(Opts) ->
	Port = proplists:get_value(port, Opts, 8080),
	case mochiweb:start_http(Port, {?MODULE, handle, []}) of
    {ok, Pid} -> {ok, Pid};
    {error,{already_started, Pid}} -> {ok, Pid}
    end.

handle(Req) ->
	Method = Req:get(method),
	case Req:get(path) of
	"/" ++ ?APIVSN ++ Path ->
		case authorized(Method, Path, Req) of
		true -> handle(Method, Path, Req);
		false -> Req:respond({401, [], "Fobbiden"})
		end;
	_ -> 
		Req:respond({404, [], "Bad Request"})
	end.

handle('POST', "/presences/online", Req) ->
	Params = params(post, Req), 
	?INFO("online: ~p", [Params]),
    Domain = get_value(<<"domain">>, Params),
    {Class, Name} = slim_id:parse(get_value(<<"name">>, Params)),
    FromOid = slim_oid:make(Class, Domain, Name),
	case slim_client:online(FromOid,  Params) of
	{ok, Data} -> 
		reply(200, Data, Req);
	{error, Code, Reason} ->
		reply(Code, Reason, Req)
	end;

handle('POST', "/presences/offline", Req) ->
	Params = params(post, Req),
    Ticket = slim_ticket:make(get_value(<<"ticket">>, Params)),
	slim_client:offline(Ticket, Params),
	reply(200, Req);

handle('POST', "/presences/update", Req) ->
	%TODO: ....
	reply(200, Req);

handle('GET', "/presences", Req) ->
	Params = params(get, Req),
    Domain = get_value(<<"domain">>, Params),
	Ids = binary:split(get_value(ids, Params), [<<",">>], [global]),
	Presences = slim_client:get_presences(Domain, Ids),
	reply(200, Presences, Req);

handle('POST', "/messages/send", Req) ->
	Params = params(post, Req),
	Ticket = slim_ticket:make(get_value(<<"ticket">>, Params)),
	case slim_client:send_message(Ticket, Params) of
	ok ->
		reply(200, Req);
	{error, Reason} ->
		reply(500, Reason, Req)
	end;

handle('POST', "/messages/push", Req) ->
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

handle('POST', "/rooms/join", Req) ->
	reply(200, Req);

handle('POST', "/rooms/leave", Req) ->
	reply(200, Req);

handle(Req, <<"POST">>, <<"/rooms/members">>) ->
	reply(200, Req);

handle(_Method, _Path, Req) ->
	reply(400, <<"Bad Request">>, Req).

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

params(get, Req) ->
	[{list_to_binary(Key), list_to_binary(Value)} 
		|| {Key, Value} <- mochiweb_request:parse_qs(Req)];

params(post, Req) ->
	[{list_to_binary(Key), list_to_binary(Value)} 
		|| {Key, Value} <- mochiweb_request:parse_post(Req)].

reply(Code, Req) when (code >= 200) and (Code < 300)  ->
	Json = slim_json:encode([{status, ok}]),
	Req:respond({Code, [{"Content-Type", "text/plain"}], Json}). 
	
reply(Code, Data, Req) when (code >= 200) and (Code < 300) ->
	Json = slim_json:encode([{status, ok}, {data, Data}]),
	Req:respond({Code, [{"Content-Type", "text/plain"}], Json});

reply(Code, Reason, Req) when (code >= 400) ->
	Json = slim_json:encode([{status, error}, {reason, Reason}]),
	Req:respond({Code, [{"Content-Type", "text/plain"}], Json}). 


