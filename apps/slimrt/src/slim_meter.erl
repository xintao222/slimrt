-module(slim_meter).

-export([incr/2, decr/2]).

incr(Meter, Domain) when is_atom(Meter) and is_binary(Domain) ->
    ok.

decr(Meter, Domain) when is_atom(Meter) and is_binary(Domain) ->
    ok.

