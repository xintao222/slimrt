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

-module(slim_jsonp).

-include("slimrt.hrl").

-import(lists, [reverse/1]).

-export([pack/1,
		valid/1,
		encode/1,
		jsonify/1]).

%TODO: Refactor Later
pack(Packets) ->
    pack(Packets, [], [], []).

pack([], MsgAcc, PresAcc, StatAcc) ->
    jsonify([{status, <<"ok">>}, 
			 {messages, reverse(MsgAcc)}, 
			 {presences, reverse(PresAcc)}, 
			 {statuses, reverse(StatAcc)}]);

pack([Message = #slim_message{from=FromOid, nick=Nick, to=ToOid, timestamp=Timestamp, 
    type=Type ,body=Body, style=Style} | T], MsgAcc, PresAcc, StatusAcc) ->
	valid(Message),
    MsgObj = [{from, slim_id:from(FromOid)},
              {to, slim_id:from(ToOid)},
			  {nick, Nick}, 
			  {timestamp, Timestamp}, 
              {type, Type},
			  {body, Body},
			  {style, Style}],
    pack(T, [MsgObj|MsgAcc], PresAcc, StatusAcc);

pack([Presence = #slim_presence{type=Type, from=FromOid, to=ToOid, nick=Nick, show=Show, 
    status=Status} | T], MsgAcc, PresAcc, StatusAcc) ->
	valid(Presence),
    PresObj = [{from, slim_id:from(FromOid)},
			   {nick, Nick},
               {type, Type}, 
			   {show, Show}, 
			   {status, Status}],
    PresObj1 = 
    if
    is_record(ToOid, slim_oid) ->
        [{to, slim_id:from(ToOid)}|PresObj];
    true ->
        PresObj
    end,
    pack(T, MsgAcc, [PresObj1|PresAcc], StatusAcc);

pack([Status = #slim_status{from= FromOid, to=ToOid, nick = Nick, show=Show} | T], 
    MsgAcc, PresAcc, StatusAcc) ->
	valid(Status),
    StatusObj = [{from, slim_id:from(FromOid)},
				 {nick, Nick},
                 {to, slim_id:from(ToOid)},
				 {show, Show}],
    pack(T, MsgAcc, PresAcc, [StatusObj|StatusAcc]).

encode(Msg) when is_record(Msg, slim_message) ->
	valid(Msg),
	jsonify(jsonobj(Msg));

encode(Presence) when is_record(Presence, slim_presence) ->
	valid(Presence),
	jsonify(jsonobj(Presence));

encode(Status) when is_record(Status, slim_status) ->
	valid(Status),
	jsonify(jsonobj(Status));

encode(L) when is_list(L) ->
	jsonify([jsonobj(E) || E <- L]).

valid(Msg = #slim_message{from=undefined}) ->
	throw({badpkt, Msg});
valid(Msg = #slim_message{to=undefined}) ->
	throw({badpkt, Msg});
valid(Msg) when is_record(Msg, slim_message) ->
	true;

valid(Presence = #slim_presence{from=undefined}) ->
	throw({badpkt, Presence});
valid(Presence) when is_record(Presence, slim_presence) ->
	true;

valid(Status = #slim_status{from=undefined}) ->
	throw({badpkt, Status});
valid(Status = #slim_status{to=undefined}) ->
	throw({badpkt, Status});
valid(Status) when is_record(Status, slim_status) ->
	true.

jsonobj(#slim_message{from=FromOid, 
						nick=Nick, 
						to=ToOid, 
						timestamp=Timestamp, 
						type=Type,
						body=Body, 
						style=Style}) ->
    [{message, [{from, slim_id:from(FromOid)}, 
				{nick, Nick}, 
				{to, slim_id:from(ToOid)}, 
				{timestamp, Timestamp}, 
				{type, Type}, 
				{body, Body}, 
				{style, Style} ]}];

jsonobj(#slim_presence{type=Type,
                        to=ToOid,
						from=FromOid,
						nick=Nick,
						show=Show,
						status=Status}) ->
    Data = [{from, slim_id:from(FromOid)},
            {nick, Nick},
            {type, Type},
            {show, Show},
            {status, Status}],
    Data1 = 
    if
    ToOid == undefined -> Data;
    true -> [{to, slim_id:from(ToOid)}|Data]
    end,
	[{presence, Data1}];

jsonobj(#slim_status{ from= FromOid,
						to=ToOid,
						nick = Nick,
						show=Show}) ->
    [{status, [{from, slim_id:from(FromOid)},
			   {nick, Nick},
			   {to, slim_id:from(ToOid)},
			   {show, Show}]}].

jsonify(Term) ->
    iolist_to_binary(mochijson2:encode(Term)).


