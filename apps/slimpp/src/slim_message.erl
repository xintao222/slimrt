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

-export([make/4, to_list/1]).

-import(slim_util, [g/2, g/3]).

-spec make(Type :: atom(), FromOid :: oid(), ToOid :: oid(), Params :: [tuple()]) -> message().
make(Type, FromOid, ToOid, Params) ->
	Id = proplists:get_value(<<"id">>, Params),
	Nick = proplists:get_value(<<"nick">>, Params, <<>>),
	Body = proplists:get_value(<<"body">>, Params),
	Ts = proplists:get_value(<<"ts">>, Params),
	ContentType = proplists:get_value(<<"content_type">>, Params, <<"text">>),
	ContentEncoding = proplists:get_value(<<"content_encoding">>, Params),
	Content = #slim_content{
		type = binary_to_atom(ContentType, utf8), 
		encoding = ContentEncoding, 
		body = Body
	},
	#slim_message {
		id			= Id,
		type 		= Type,
		from		= FromOid,
		nick		= Nick,
		to			= ToOid,
		ts			= Ts, 
		content		= Content
	}.

-spec to_list(Message :: message()) -> [ term() ].
to_list(#slim_message {
	id			= Id,
	from		= FromOid,
	nick		= Nick,
	to			= ToOid,
	ts			= Ts, 
	type 		= Type,
	content		= Content}) ->
	[{id, Id},
	 {type, Type},
	 {from, slim_id:from(FromOid)},
	 {nick, Nick},
	 {to, slim_id:from(ToOid)},
	 {ts, Ts},
	 {content, to_list(Content)}];

to_list(#slim_content{
		type = text, 
		body = Body
	}) ->
	Body;

to_list(#slim_content{type = Type, encoding = undefined, body = Body}) ->
	[{type, Type}, {body, Body}];
to_list(#slim_content{type = Type, encoding = Encoding, body = Body}) ->
	[{type, Type}, {encoding, Encoding}, {body, Body}].


