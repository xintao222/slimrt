-module(slim_id).

-export([parse/1, from/1]).

parse(S) when is_binary(S) ->
	case binary:split(S, [<<":">>]) of
	[Cls, Id] -> {binary_to_atom(Cls), Id};
	[Id] -> {uid, Id}
	end.

from(#nextalk_oid{class=uid, name=Name}) ->
	Name;
from(#nextalk_oid{class=gid, name=Name}) ->
	Name;
from(#nextalk_oid{class=Cls, name=Name}) ->
	list_to_binary([atom_to_list(Cls), ":", Name]).



