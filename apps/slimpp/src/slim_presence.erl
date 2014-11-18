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

-import(slim_util, [g/2]).

-export([make/3, to_list/1]).

make(Type, FromOid, Params) when is_atom(Type) and is_record(FromOid, slim_oid) ->
	Nick = proplists:get_value(<<"nick">>, Params),
	Show = proplists:get_value(<<"show">>, Params),
	Status = proplists:get_value(<<"status">>, Params),
	#slim_presence {
		type = Type, 
		from = FromOid, 
		nick = Nick, 
		show = Show, 
		status = Status
	}.

to_list(#slim_presence{
	type = Type, 
	from = FromOid, 
	to = ToOid, 
	nick = Nick, 
	show = Show, 
    status = Status}) -> 
    List = [
		{from, slim_id:from(FromOid)},
		{nick, Nick},
        {type, Type}, 
		{show, Show}, 
		{status, Status}],
    if
    is_record(ToOid, slim_oid) ->
        [{to, slim_id:from(ToOid)} | List];
    true ->
        List
	end.
	
