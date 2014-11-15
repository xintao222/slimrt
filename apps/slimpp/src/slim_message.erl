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

-module(slim_message).

-include("slimpp.hrl").

-export([make/3, to_list/1]).

-spec make(FromOid :: oid(), ToOid :: Oid(), Params :: [tuple()]) -> message().
make(FromOid, ToOid, Params) ->
    Domain = g(<<"domain">>, Params),
	%User Oid
	{Ticket, UserOid} =
	case g(<<"ticket">>, Params) of
	undefined -> %Push Message
		{FromCls, From} = slim_id:parse(g(<<"from">>, Params)),
		{undefined, slim_oid:make(FromCls, Domain, From)};
	S -> %Send Message
		T = slim_ticket:make(S),
		{T, makeoid(Domain, T)}
	end,
	%Message
	{ToCls, To} = slim_id:parse(g(<<"to">>, Params)),
	Body = g(<<"body">>, Params),
	Nick = g(<<"nick">>, Params, <<>>),
	Style = g(<<"style">>, Params, <<>>),
	Timestamp = g(<<"timestamp">>, Params),
	Type = binary_to_atom(g(<<"type">>, Params, <<"chat">>)),
	ToOid = 
	case Type of
	chat -> slim_oid:make(ToCls, Domain, To);
	grpchat -> slim_oid:make(gid, Domain, To)
	end,
	#slim_message {
		id			= Id,
		from		= FromOid,
		nick		= Nick,
		to			= ToOid,
		ts			= Ts, 
		type 		= Type,
		content		= Content
	} = Message,
	#slim_message{nick=Nick,
				  to=ToOid,
				  type=Type,
				  body=Body,
				  style=Style,
				  timestamp=Timestamp}.

makeoid(Domain, #slim_ticket{class=Cls, name=Name}) ->
	slim_oid:make(Cls, Domain, Name).

-spec to_list(Message :: message()) -> [ term() ].
to_list(Message) when is_record(Message, slim_message) ->
	#slim_message {
		id			= Id,
		from		= FromOid,
		nick		= Nick,
		to			= ToOid,
		ts			= Ts, 
		type 		= Type,
		content		= Content
	} = Message,
	[{id, Id}
	 {type, Type},
	 {from, slim_id:from(FromOid)},
	 {nick, Nick},
	 {to, slim_id:from(ToOid)},
	 {ts, Ts},
	 {content, to_list(Content)}].

-spec to_list(Content :: content()) -> [ tuple() ] | binary.
to_list(#slim_content{type = text, body = Body}) ->
	Body;
to_list(#slim_content{type = Type, encoding = undefined, body = Body}) ->
	[{type, Type}, {body, Body}];
to_list(#slim_content{type = Type, encoding = Encoding, body = Body}) ->
	[{type, Type}, {encoding, Encoding}, {body, Body}].

