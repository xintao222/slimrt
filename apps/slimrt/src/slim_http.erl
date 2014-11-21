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

-export([init/2,
		 terminate/3]).  

%TODO: Refactor this file later.

init(Req, Opts) ->
	?INFO("Opts: ~p", [Opts]), %% what's in Opts??
	case authorized(Req) of
	true ->
		{ok, handle(Req, Opts), Opts};
	{false, Error} ->
		{ok, reply(401, Error, Req), Opts}
	end.

authorized(Req) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"basic">>, {Domain, ApiKey}}} ->
        case slim_auth:check(Domain, ApiKey) of
        true -> true;
        {error, Error} -> {false, Error}
        end;
    _Auth ->
        {false, <<"Fobbiden">>}
    end.

%%TODO: opts not used??
handle(Req, _Opts) ->
	Method = cowboy_req:method(Req),
	case cowboy_req:path(Req) of
	<<"/", ?APIVSN, Path/binary>> ->
		handle(Req, Method, Path);
	_ ->
		reply(400, <<"Bad Path">>, Req)
	end.

handle(Req, <<"POST">>, <<"/presences/online">>) ->
	{ok, Params, Req1} = cowboy_req:body_qs(Req),
    Domain = get_value(<<"domain">>, Params),
    {Class, Name} = slim_id:parse(get_value(<<"name">>, Params)),
    FromOid = slim_oid:make(Class, Domain, Name),
	case slim_client:online(FromOid,  Params) of
	{ok, Data} -> 
		reply(200, Data, Req1);
	{error, Code, Reason} ->
		reply(Code, Reason, Req1)
	end;

handle(Req, <<"POST">>, <<"/presences/offline">>) ->
	{ok, Params, Req1} = cowboy_req:body_qs(Req),
    Ticket = slim_ticket:make(get_value(<<"ticket">>, Params)),
	slim_client:offline(Ticket, Params),
	reply(200, Req1);

handle(Req, <<"POST">>, <<"/presences/set">>) ->
	%TODO: ....
	reply(200, Req);

handle(Req, <<"GET">>, <<"/presences">>) ->
	{ok, Params} = cowboy_req:qs_vals(Req),
    Domain = get_value(<<"domain">>, Params),
	Ids = binary:split(get_value(ids, Params), [<<",">>], [global]),
	Presences = slim_client:get_presences(Domain, Ids),
	reply(200, Presences, Req);

handle(Req, <<"POST">>, <<"/messages/send">>) ->
	{ok, Params, Req1} = cowboy_req:body_qs(Req),	
	Ticket = slim_ticket:make(get_value(<<"ticket">>, Params)),
	case slim_client:send_message(Ticket, Params) of
	ok ->
		reply(200, Req1);
	{error, Reason} ->
		reply(500, Reason, Req1)
	end;

handle(Req, <<"POST">>, <<"/messages/push">>) ->
	{ok, Params, Req1} = cowboy_req:body_qs(Req),	
	Domain = get_value(<<"domain">>, Params),
	{FromCls, From} = slim_id:parse(get_value(<<"from">>, Params)),
	FromOid = slim_oid:make(FromCls, Domain, From),
	case slim_client:push_message(FromOid, Params) of
	ok ->
		reply(200, Req1);
	{error, Reason} ->
		reply(500, Reason, Req1)
	end;

handle(Req, <<"POST">>, <<"/rooms/join">>) ->
	reply(200, Req);

handle(Req, <<"POST">>, <<"/rooms/leave">>) ->
	reply(200, Req);

handle(Req, <<"POST">>, <<"/rooms/members">>) ->
	reply(200, Req);

handle(Req, _Method, _Path) ->
	reply(400, <<"Bad Request">>, Req).

terminate(_Reason, _Req, _Opts) ->
	ok.

reply(Code, Req) when (code >= 200) and (Code < 300)  ->
	Json = slim_json:encode([{status, ok}]),
	cowboy_req:reply(Code, [], Json, Req). 
	
reply(Code, Data, Req) when (code >= 200) and (Code < 300) ->
	Json = slim_json:encode([{status, ok}, {data, Data}]),
	cowboy_req:reply(Code, [], Json, Req);

reply(Code, Reason, Req) when (code >= 400) and is_binary(Reason) ->
	Json = slim_json:encode([{status, error}, {reason, Reason}]),
	cowboy_req:reply(Code, [], Json, Req).


