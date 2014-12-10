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

-module(slim_ticket).

-include("slimpp.hrl").

-export([make/1, make/2, make/3,
		 encode/1,
		 s/1]).

%%
%% @doc gererate ticket record from string
%%
-spec make(Ticket :: binary()) -> ticket().
make(Ticket) when is_binary(Ticket) ->
	[Class, Id, Token] = binary:split(Ticket, <<":">>, [global]), 
	#slim_ticket{class=binary_to_atom(Class, utf8), id = Id, token = Token}.
    
%%
%% @doc generate ticket with class and id
%%
-spec make(Class :: oid_class(), Id :: binary()) -> ticket().
make(Class, Id) when is_atom(Class) and is_binary(Id) ->
	make(Class, Id, token()).

%%
%% @doc generate ticket with class, id and token
%%
-spec make(Class :: oid_class(), Id :: binary(), Token :: binary()) 
	-> ticket().
make(Class, Id, Token) when is_atom(Class) 
	and is_binary(Id) and is_binary(Token) ->
	#slim_ticket{class = Class, id = Id, token = Token}.

-spec encode(Ticket :: ticket()) -> binary().
encode(#slim_ticket{class = Class, id = Id, token = Token}) ->
	list_to_binary([atom_to_list(Class), ":", Id, ":", Token]).

token() ->
	random:seed(now()),
    I1 = random:uniform(round(math:pow(2, 48))) - 1, 
	I2 = random:uniform(round(math:pow(2, 32))) - 1, 
	L = lists:flatten(io_lib:format("~12.16.0b~8.16.0b", [I1, I2])),
	list_to_binary(L).

s(Ticket) when ?is_ticket(Ticket) ->
	encode(Ticket).

