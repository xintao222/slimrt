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

-module(slim_packet_tests).

-include("slimpp.hrl").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_packets_test() ->
    Json1 = slim_packet:encode([new_presence()]),
    ?debugFmt("~p~n", [Json1]),

    Json2 = slim_packet:encode([new_message(), new_presence()]),
    ?debugFmt("~p~n", [Json2]).

encode_error_test() ->
	Err = #slim_error{code = 404, reason = <<"Not Found.">>},
	Json = slim_packet:encode(Err),
	?debugMsg(Json).

new_message() ->
	FromOid = #slim_oid{domain = <<"localhost">>, class = uid, id = <<"uid1">>},
	ToOid = #slim_oid{domain = <<"localhost">>, class = uid, id = <<"uid2">>},
	Params = [
		{<<"id">>, <<"13288371">>},
		{<<"chatid">>, <<"183838">>},
		{<<"nick">>, <<"User1">>},
		{<<"subject">>, <<"Hello">>},
		{<<"body">>, <<"Hello world">>},
		{<<"ts">>, <<"1923837771">>}
	],
	slim_message:make(chat, FromOid, ToOid, Params).

new_presence() ->
    FromOid = #slim_oid{domain = <<"localhost">>, class = vid, id = <<"v1291">>},
    Params = [
        {<<"nick">>, <<"V1291">>},
        {<<"show">>, <<"available">>},
        {<<"status">>, <<"#">>}
    ],
    slim_presence:make(online, FromOid, Params).

-endif.
