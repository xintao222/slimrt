%%-----------------------------------------------------------------------------
%% Copyright (c) 2014, Feng Lee <feng@slimchat.io>
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

-module(slim_presence_tests).

-include("slimpp.hrl").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

make_test() ->
    Presence = new_presence(),
    ?debugFmt("~p", [Presence]),
    ?assertMatch(#slim_presence{ show = <<"available">> }, Presence).

new_presence() ->
    FromOid = #slim_oid{domain = <<"localhost">>, class=vid, id = <<"v1291">>},
    Params = [
        {<<"nick">>, <<"V1291">>},
        {<<"show">>, <<"available">>},
        {<<"status">>, <<"#">>}
    ],
    slim_presence:make(online, FromOid, Params).
    
list_test() ->
    List = slim_presence:list(new_presence()),
    ?debugFmt("~p", [List]),
    ?assertEqual(<<"V1291">>, proplists:get_value(nick, List)),
    ?assertEqual(<<"available">>, proplists:get_value(show, List)).

-endif.

