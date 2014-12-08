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

-module(slim_topic_tests).

-include("slimpp.hrl").

-import(slim_topic, [validate/1, type/1, match/2, triples/1, words/1]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

validate_test() ->
	?assert( validate({subscribe, <<"a/b/c">>}) ),
	?assert( validate({subscribe, <<"/a/b">>}) ),
	?assert( validate({subscribe, <<"/+/x">>}) ),
	?assert( validate({subscribe, <<"/a/b/c/#">>}) ),
	?assertNot( validate({subscribe, <<"a/#/c">>}) ).

type_test() ->
	?assertEqual(direct, type(#topic{name = <<"/a/b/cdkd">>})),
	?assertEqual(wildcard, type(#topic{name = <<"/a/+/d">>})),
	?assertEqual(wildcard, type(#topic{name = <<"/a/b/#">>})).

match_test() ->
	?assert( match(<<"/a/b/c">>, <<"/a/b/c">>) ),
	?assert( match(<<"/aa/bb/cc">>, <<"/aa/#">>) ),
	?assert( match(<<"/aa/bb/ddd/cc">>, <<"/aa/+/+/cc">>) ).

triples_test() ->
	?debugFmt("triples: ~p~n", [triples(<<"aa/bb/cc">>)]),
	?debugFmt("triples: ~p~n", [triples(<<"/aa/bb/cc">>)]).

words_test() ->
	?debugFmt("words: ~p~n", [words(<<"/x/y/zz/#">>)]).

-endif.


