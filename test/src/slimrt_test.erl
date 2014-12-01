-module(slimrt_test).

-export([start/1]).

-define(DOMAIN, "localhost").
-define(APIKEY, "public").
-define(SERVER, "http://localhost:8080/v1").

start([Max]) ->
    inets:start(permanent),
    Num = list_to_integer(atom_to_list(Max)), 
    run(Num, Num).

run(0, _Max) ->
	done;

run(I, Max) ->
    Name = integer_to_list(I),
    Nick = lists:concat(["user", Name]),
    Buddies = [integer_to_list(random:uniform(Max)) || _I <- lists:seq(1, 20)],
    Rooms = [integer_to_list(random:uniform(Max)) || _I <- lists:seq(1, 10)],
    Params = [
        {"domain", ?DOMAIN}, 
        {"name", Name}, 
        {"nick", Nick}, 
        {"buddies", string:join(Buddies, ",")}, 
        {"rooms", string:join(Rooms, ",")}
    ],
    Body = string:join([lists:concat([K, "=", V]) || {K, V} <- Params], "&"),
    Request = {?SERVER ++ "/online", headers(), "application/x-www-form-urlencoded", Body},
    io:format("~p~n", [Request]),
    Profile = profile(I),
    inets:start(httpc, [{profile, Profile}, {timeout, 120000}]),
    case httpc:request(post, Request, [{timeout, 30000}], [], Profile) of
    {ok, {_Status, _Headers, Resp}} ->
		io:format("~s~n", [Resp]),
        {struct, JSON} = mochijson2:decode(Resp),
        io:format("start loop ~p ~n", [I]),
        spawn(fun() -> loop(JSON, Profile) end);
    {error, Reason} ->
        io:format("Error: ~p~n", [Reason])
    end,
    timer:sleep(400),
    run(I-1, Max).

loop(JSON, Profile) ->
	io:format("~p~n", [JSON]),
	{struct, Data} = proplists:get_value(<<"data">>, JSON),
    {struct, Server} = proplists:get_value(<<"server">>, Data),
	Jsonp = binary_to_list(proplists:get_value(<<"jsonp">>, Server)),
    Ticket = binary_to_list(proplists:get_value(<<"ticket">>, Data)),
    Params = [{"domain", ?DOMAIN}, {"ticket", Ticket}],
    Query = string:join([lists:concat([K, "=", V]) || {K, V} <- Params], "&"),
    Url = Jsonp ++ "?" ++ Query,
    io:format("poll: ~s~n", [Ticket]),
    case httpc:request(Url, Profile) of
    {error, Reason} ->
        io:format("~s poll error: ~p~n", [Profile, Reason]);
    {ok, {_Status, _Headers, Body}} ->
        io:format("~s loop response: ~p~n", [Profile, Body]),
        timer:sleep(5),
        loop(JSON, Profile)
    end.

profile(I) ->
    list_to_atom("httpc_" ++ integer_to_list(I)).

headers() ->
    Auth = base64:encode_to_string(?DOMAIN ++ ":" ++ ?APIKEY), 
    [{"Authorization", "Basic " ++  Auth}].
    

