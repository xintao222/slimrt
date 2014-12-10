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

-module(slim_oid).

-include("slimpp.hrl").

-export([make/1, make/2, make/3,
         class/1,
         domain/1,
         id/1,
         topic/1,
		 s/1]).

%Example: {<<"/dn/$domain/$class/$id">>, <<"$domain">>, $class, <<"$id">>}

%%
%% @doc Make an oid with class, domain, id.
%%
-spec make({Class :: oid_class(), Domain :: binary(), Id :: binary()}) -> oid().
make({Class, Domain, Id}) when is_atom(Class) and is_binary(Domain) and is_binary(Id) ->
    make(Class, Domain, Id).

%%
%% @doc Make an oid with domain, ticket
%%
-spec make(Domain :: binary(), ticket()) -> oid().
make(Domain, #slim_ticket{class=Class, id=Id}) ->
    make(Class, Domain, Id).

%%
%% @doc Make an oid with class, domain, id.
%%
-spec make(Class :: oid_class(), Domain :: binary(), Id :: binary()) -> oid().
make(Class, Domain, Id) when is_atom(Class) and is_binary(Domain) and is_binary(Id) ->
    #slim_oid{domain = Domain, class = Class, id = Id}.

-spec class(Oid :: oid()) -> atom().
class(Oid) when ?is_oid(Oid) -> Oid#slim_oid.class.

-spec domain(Oid :: oid()) -> binary().
domain(Oid) when is_record(Oid, slim_oid) -> Oid#slim_oid.domain.

-spec id(Oid :: oid()) -> binary().
id(Oid) when is_record(Oid, slim_oid) -> Oid#slim_oid.id.

-spec topic(Oid :: oid()) -> binary().
topic(#slim_oid{domain=Domain, class=Class, id=Id}) ->
    list_to_binary(["/dn/", Domain, "/", atom_to_list(Class), "/", Id]).

-spec s(Oid :: oid()) -> binary().
s(Oid) -> topic(Oid).


