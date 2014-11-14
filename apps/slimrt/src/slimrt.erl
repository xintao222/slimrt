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

-module(slimrt).

%% API.
-export([start/0]).

-export([cluster/1,
         vmstats/0,
		 metrics/0,
		 port/1,
		 server/0,
		 server/1,
         isopened/1]).

%% API.

start() ->
	application:start(ranch),
	application:start(slimrt).

cluster(Node) ->
    case net_adm:ping(Node) of
    pong ->
        application:stop(slimrt),
        mnesia:stop(),
        mnesia:start(),
        mnesia:change_config(extra_db_nodes, [Node]),
        application:start(slimrt),
        ?PRINT("cluster with ~p successfully.~n", [Node]);
    pang ->
        ?PRINT("failed to connect to ~p~n", [Node])
    end.

-spec metrics() -> [tuple()].
metrics() ->
    [{M, folsom_metrics:get_metric_value(M)} 
        || M <- folsom_metrics:get_metrics()].

vmstats() ->
    [{memory, erlang:memory()},
     {tables, tables()}, 
	 {load1, cpu_sup:avg1()/256},
	 {load5, cpu_sup:avg5()/256},
	 {load15, cpu_sup:avg15()/256},
	 {process_limit, erlang:system_info(process_limit)},
	 {process_count, erlang:system_info(process_count)},
	 {max_fds, proplists:get_value(max_fds, erlang:system_info(check_io))}].

tables() ->
    case mnesia:system_info(is_running) of
    yes -> [{Tab, mnesia:table_info(Tab, size)} || Tab <- mnesia:system_info(tables)];
    _   -> []
    end.

-spec port(Name :: httpd | mqttd) -> hon_neg_integer.
port(Name) when is_atom(Name) ->
	{ok, Opts} = application:get_env(slimrt, Name),
	slimrt_misc:g(port, Opts).

-spec server() -> string().
server() ->
	{ok, Server} = application:get_env(slimrt, server), Server.

-spec server(Name :: httpd | mqttd) -> non_neg_integer.
server(Name) when is_atom(Name) ->
	list_to_binary([server(), ":", integer_to_list(port(Name))]).

-spec isopened(Name :: httpd | mqttd) -> non_neg_integer.
isopened(Name) ->
	{ok, Opts} = application:get_env(slimrt, Name),
	slimrt_misc:g(open, Opts, true).

