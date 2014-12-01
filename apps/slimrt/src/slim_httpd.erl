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

-module(slim_httpd).

-author('feng.lee@slimchat.io').

-include("slim_api.hrl").

-include("slim_log.hrl").

-export([start/1]).

%% External API
start(Opts) ->
	Handler = fun(API) -> {lists:concat(["/", ?APIVSN, API]), slim_http, []} end,
	Dispatch = cowboy_router:compile([
		{'_', [
			%Jsonp long poll
			{lists:concat(["/", ?APIVSN, ?SLIM_JSONP_API]), slim_jsonp, []},
			%Websocket long connect
			{lists:concat(["/", ?APIVSN, ?SLIM_WSOCKET_API]), slim_wsocket, []} 
			| [ Handler(API) || API <- ?SLIM_HTTP_APIS ]
		]}
	]),
	cowboy:start_http(test_http, 2, [{port, 8088}], []),
	%% Name, NbAcceptors, TransOpts, ProtoOpts
	HttpdPid = 
	case cowboy:start_http(?MODULE, 2, Opts, [{env, [{dispatch, Dispatch}]}]) of
    {ok, Pid} -> Pid;
    {error,{already_started, Pid}} -> Pid
    end,
	Port = proplists:get_value(port, Opts),
	?PRINT("Slim Httpd is listening on ~p~n", [Port]),
	{ok, HttpdPid}.

