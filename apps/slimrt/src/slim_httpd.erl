-module(slim_httpd).

-author('feng.lee@slimchat.io').

-include("slimrt.hrl").

-include("slim_log.hrl").

-export([start/1]).

%% External API
start(Opts) ->
	Handler = fun(API) -> {lists:concat(["/", ?APIVSN, API]), slim_http, []} end,
	Dispatch = cowboy_router:compile([
		{'_', [
			%Jsonp long poll
			{lists:concat(["/", ?APIVSN, ?API_JSONP]), slim_jsonp, []},
			%Websocket long connect
			{lists:concat(["/", ?APIVSN, ?API_WSOCKET]), slim_wsocket, []} 

			| [ Handler(API) || API <- ?HTTP_APIS ]
		]}
	]),
	%% Name, NbAcceptors, TransOpts, ProtoOpts
	HttpdPid = case cowboy:start_http(?MODULE, 100, Opts, [{env, [{dispatch, Dispatch}]}]) of
    {ok, Pid} -> Pid;
    {error,{already_started, Pid}} -> Pid
    end,
	Port = proplists:get_value(port, Opts),
	?PRINT("NexTalk Httpd is listening on ~p~n", [Port]),
	{ok, HttpdPid}.

