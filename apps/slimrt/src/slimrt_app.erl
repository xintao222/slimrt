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

-module(slimrt_app).

-include("slim_log.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
	print_banner(),
	{ok, Sup} = slimrt_sup:start_link(),
	start_services(Sup),
	slim_http:start(env(httpd)),
    %start mqtt
    MqttEnv = env(mqttd),
    case proplists:get_value(open, MqttEnv, false) of
    true -> slim_mqtt:start(MqttEnv);
    false -> ok
    end,
	print_vsn(),
	{ok, Sup}.

env(Key) ->
	{ok, Opts} = application:get_env(Key), Opts.

print_banner() ->
	todo.

print_vsn() ->
	{ok, Vsn} = application:get_key(vsn),
	{ok, Desc} = application:get_key(description),
	?PRINT("~s ~s is running now~n", [Desc, Vsn]).

start_services(Sup) ->
	lists:foreach(
        fun({Name, F}) when is_function(F) ->
			?PRINT("~s is starting...", [Name]),
            F(),
			?PRINT_MSG("[done]~n");
		   ({Name, Server}) when is_atom(Server) ->
			?PRINT("~s is starting...", [Name]),
			start_child(Sup, Server),
			?PRINT_MSG("[done]~n");
           ({Name, Server, Opts}) when is_atom(Server) ->
			?PRINT("~s is starting...", [ Name]),
			start_child(Sup, Server, Opts),
			?PRINT_MSG("[done]~n")
		end,
	 	[{"Slim PubSub", slim_pubsub},
         {"Slim Router", slim_router},
         %{"Slim Auth", nextalk_auth},
		 {"Slim Roster", slim_roster},
		 {"Slim GrpChat", slim_grpchat},
		 {"Slim Monitor", slimrt_monitor},
         {"Slim Client Manager", slim_cm},
		 {"Slim Endpoint Supervisor", fun() -> 
            Name = slim_endpoint_sup,
            supervisor:start_child(Sup,
                {Name, {Name, start_link, []}, 
                    permanent, 10, supervisor, [Name]})
         end}
		]).

start_child(Sup, Name) ->
    {ok, _ChiId} = supervisor:start_child(Sup, worker_spec(Name)).
start_child(Sup, Name, Opts) ->
    {ok, _ChiId} = supervisor:start_child(Sup, worker_spec(Name, Opts)).

worker_spec(Name) ->
    {Name, {Name, start_link, []}, 
        permanent, 5000, worker, [Name]}.
worker_spec(Name, Opts) ->
    {Name, {Name, start_link, [Opts]}, 
        permanent, 5000, worker, [Name]}.

stop(_State) ->
    ok.
