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

-module(slim_presence).

-include("slimpp.hrl").

-export([make/3, list/1]).

make(Type, FromOid, Params) when is_atom(Type) and is_record(FromOid, slim_oid) ->
	Presence = #slim_presence{type = Type, from = FromOid},
	make(Params, Presence).

make([], Presence) ->
	Presence;
make([{<<"nick">>, Nick} | Params], Presence) ->
	make(Params, Presence#slim_presence{nick = Nick});
make([{<<"show">>, Show} | Params], Presence) ->
	make(Params, Presence#slim_presence{show = Show});
make([{<<"status">>, Status} | Params], Presence) ->
	make(Params, Presence#slim_presence{status = Status});
make([_ | Params], Presence) ->
	make(Params, Presence).
	
list(Presence) when is_record(Presence, slim_presence) ->
	[{K, V} || {K, V} <- ?record_to_list(slim_presence, Presence), V =/= undefined].

