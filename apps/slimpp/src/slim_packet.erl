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

-module(slim_packet).

-include("slimpp.hrl").

-export([encode/1, validate/1, uniform/1]).

-import(lists, [reverse/1]).

encode(Packets) when is_list(Packets) and length(Packets) > 0  ->
	DataMap = encode(Packets, #{messages => [], presences => []}),
	Data = [{K, V} || {K, V} <- maps:to_list(DataMap), V =/= []],
	jsonify([{data, Data}]);

encode(#slim_error{code = Code, reason = Reason}) ->
	jsonify([{error, [{code, Code}, {reason, Reason}]}]).

encode([], AccMap = #{messages := MsgAcc, presences := PresAcc}) ->
	AccMap#{messages := reverse(MsgAcc), presences := reverse(PresAcc)};

encode([Msg|T], AccMap = #{messages := MsgAcc}) when is_record(Msg, slim_message) ->
	validate(Msg),
	encode(T, AccMap#{messages := [uniform(slim_message:list(Msg))|MsgAcc]});

encode([Pres|T], AccMap = #{presences := PresAcc}) when is_record(Pres, slim_presence) ->
	validate(Pres),
	encode(T, AccMap#{presences := [uniform(slim_presence:list(Pres))|PresAcc]}).

validate(Msg = #slim_message{from = undefined}) ->
	throw({badpkt, Msg});
validate(Msg = #slim_message{to = undefined}) ->
	throw({badpkt, Msg});
validate(Msg) when is_record(Msg, slim_message) ->
	true;
validate(Presence = #slim_presence{from = undefined}) ->
	throw({badpkt, Presence});
validate(Presence) when is_record(Presence, slim_presence) ->
	true.

uniform(Packet) when is_list(Packet) ->
	lists:reverse(uniform(Packet, [])).

uniform([], Acc) ->
	Acc;
uniform([{_, undefined}|T], Acc) ->
	uniform(T, Acc);
uniform([{from, FromOid}|T], Acc) ->
	uniform(T, [{from, slim_id:from(FromOid)} | Acc]);
uniform([{to, ToOid}|T], Acc) ->
	uniform(T, [{to, slim_id:from(ToOid)}|Acc]);
uniform([{Key, Val}|T], Acc) ->
	uniform(T, [{Key, Val}|Acc]).

jsonify(Term) ->
    iolist_to_binary(mochijson2:encode(Term)).

