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

-export([make/4, list/1]).

-spec make(Type :: atom(), FromOid :: oid(), ToOid :: oid(), Params :: [tuple()]) -> message().
make(Type, FromOid, ToOid, Params) when is_atom(Type) and is_record(FromOid, slim_oid) 
	and is_record(ToOid, slim_oid) ->
	Message = #slim_message{type = Type, from = FromOid, to = ToOid},
	make(Params, Message).

make([], Message) ->
	Message;
make([{<<"id">>, Id} | Params], Message) ->
	make(Params, Message#slim_message{id = Id});
make([{<<"chatid">>, ChatId} | Params], Message) ->
	make(Params, Message#slim_message{chatid = ChatId});
make([{<<"nick">>, Nick} | Params], Message) ->
	make(Params, Message#slim_message{nick = Nick});
make([{<<"format">>, Format} | Params], Message) ->
	make(Params, Message#slim_message{format = binary_to_atom(Format, utf8)});
make([{<<"encoding">>, Encoding} | Params], Message) ->
	make(Params, Message#slim_message{encoding = Encoding});
make([{<<"subject">>, Subject} | Params], Message) ->
	make(Params, Message#slim_message{subject = Subject});
make([{<<"body">>, Body} | Params], Message) ->
	make(Params, Message#slim_message{body = Body});
make([{<<"ts">>, Ts} | Params], Message) ->
	make(Params, Message#slim_message{ts = binary_to_integer(Ts)});
make([_ | Params], Message) ->
	make(Params, Message).

-spec list(Message :: message()) -> [ tuple() ].
list(Message) when is_record(Message, slim_message) ->
	[{K, V} || {K, V} <- ?record_to_list(slim_message, Message), V =/= undefined].

