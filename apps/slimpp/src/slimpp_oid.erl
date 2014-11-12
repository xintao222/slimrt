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

-module(slimpp_oid).

-include("slimpp.hrl").

-export([make/1, make/3,
         class/1,
         domain/1,
         name/1,
         topic/1]).

%Example: {<<"/domain/$domain/$class/$id">>, <<"$domain">>, $class, <<"$id">>}

%%
%% @doc Make a oid with class, domain, name.
%%
-spec make({Class :: oid_class(), Domain :: binary(), Name :: binary()}) -> oid().
make({Class, Domain, Name}) when is_atom(Class) and is_binary(Domain) and is_binary(Name) ->
    make(Class, Domain, Name).

%%
%% @doc Make a oid with class, domain, name.
%%
-spec make(Class :: oid_class(), Domain :: binary(), Name :: binary()) -> oid().
make(Class, Domain, Name) when is_atom(Class) and is_binary(Domain) and is_binary(Name) ->
    #slimpp_oid{domain = Domain, class = Class, name = Name}.

-spec class(Oid :: oid()) -> atom().
class(Oid) when is_record(Oid, slimpp_oid) -> Oid#slimpp_oid.class.

-spec domain(Oid :: oid()) -> binary().
domain(Oid) when is_record(Oid, slimpp_oid) -> Oid#slimpp_oid.domain.

-spec name(Oid :: oid()) -> binary().
name(Oid) when is_record(Oid, slimpp_oid) -> Oid#slimpp_oid.name.

-spec topic(Oid :: oid()) -> binary().
topic(#slimpp_oid{domain=Domain, class=Class, name=Name}) ->
    list_to_binary(["/domain/", Domain, "/", atom_to_list(Class), "/", Name]).


