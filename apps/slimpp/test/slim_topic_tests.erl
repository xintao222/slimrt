-module(slim_topic_tests).

-include("slimpp.hrl").

-import(slim_topic, [validate/1, match/2, triples/1, words/1]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

validate_test() ->
	?assert( validate({subscribe, <<"a/b/c">>}) ),
	?assert( validate({subscribe, <<"/a/b">>}) ),
	?assert( validate({subscribe, <<"/+/x">>}) ),
	?assert( validate({subscribe, <<"/a/b/c/#">>}) ),
	?assertNot( validate({subscribe, <<"a/#/c">>}) ).

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


