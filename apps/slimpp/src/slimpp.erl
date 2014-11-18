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

-module(slimpp).

-include("slimpp.hrl").

-export([encode/1,
		 validate/1,
		 jsonify/1]).

-import(lists, [reverse/1]).

%% API
encode(#slim_error{code = Code, reason = Reason}) ->
	jsonify([{error, [{code, Code}, {reason, Reason}]}]);	

encode(Packets) when is_list(Packets) and ( length(Packets) > 0 ) ->
	DataMap = encode(Packets, #{messages => [], presences => [], statuses => []}),
	Data = [{K, V} || {K, V} <- maps:to_list(DataMap), V =/= []],
	jsonify([{data, Data}]).

encode([], AccMap = #{messages := MsgAcc, presences := PresAcc, statuses := StatusAcc}) ->
	AccMap#{messages := reverse(MsgAcc), 
			presences := reverse(PresAcc), 
			statuses := reverse(StatusAcc)
	};

encode([Msg|T], AccMap = #{messages := MsgAcc}) when is_record(Msg, slim_message) ->
	encode(T, AccMap#{messages := [slim_message:to_list(Msg)|MsgAcc]});

encode([Pres|T], AccMap = #{presences := PresAcc}) when is_record(Pres, slim_presence) ->
	encode(T, AccMap#{presences := [slim_presence:to_list(Pres)|PresAcc]});

encode([Status|T], AccMap = #{statuses := StatusAcc}) when is_record(Status, slim_status) ->
	encode(T, AccMap#{statuses := [slim_status:to_list(Status)|StatusAcc]}).
	
validate(Msg = #slim_message{from = undefined}) ->
	throw({badpkt, Msg});
validate(Msg = #slim_message{to = undefined}) ->
	throw({badpkt, Msg});
validate(Msg) when is_record(Msg, slim_message) ->
	true;
validate(Presence = #slim_presence{from = undefined}) ->
	throw({badpkt, Presence});
validate(Presence) when is_record(Presence, slim_presence) ->
	true;
validate(Status = #slim_status{from = undefined}) ->
	throw({badpkt, Status});
validate(Status = #slim_status{to = undefined}) ->
	throw({badpkt, Status});
validate(Status) when is_record(Status, slim_status) ->
	true.
	
jsonify(Term) ->
    iolist_to_binary(mochijson2:encode(Term)).

%% End of Module.
