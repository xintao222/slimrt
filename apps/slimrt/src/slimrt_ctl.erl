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

-module(slimrt_ctl).

-author('feng.lee@slimchat.io').

-export([status/1, 
        vmstats/1,
		metrics/1, 
		cluster/1, 
		cluster_info/1, 
		mnesia/1]).  

%%
%% @doc slimrt status
%% 
status([]) ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    ?PRINT("Status: ~p.~nNode ~p is ~p.~n",
              [ProvidedStatus, node(), InternalStatus]),
    ?PRINT("Running applications: ~p ~n", [application:which_applications()]).


%%
%% @doc slimrt vmstats
%%
-spec vmstats(list()) -> ok.
vmstats([]) ->
    Stats = slimrt:vmstats(),
	?PRINT("~p~n", [Stats]).

%%
%% @doc slimrt metrics
%% 
-spec metrics(list()) -> ok.
metrics([]) ->
	Metrics = slimrt:metrics(),
	?PRINT_MSG("Metrics: ~n"),
	?PRINT("~p~n", [Metrics]).

%% 
%% @doc cluster with other node
%%
-spec cluster(list(binary())) -> ok.
cluster([SNode]) ->
	Node = node_name(SNode),
    slimrt:cluster(Node).

cluster_info([]) ->
	Nodes = [node()|nodes()],
	?PRINT("cluster nodes: ~p~n", [Nodes]).

mnesia([Arg]) ->
    ?PRINT("~p ~n", [mnesia:info()]), 
    case catch mnesia:system_info(list_to_atom(Arg)) of
	{'EXIT', Error} -> ?PRINT("Error: ~p~n", [Error]);
	Return -> ?PRINT("~p~n", [Return])
    end.

node_name(SNode) ->
    SNode1 = 
    case string:tokens(SNode, "@") of
    [_Node, _Server] ->
        SNode;
    _ ->
        case net_kernel:longnames() of
         true ->
             SNode ++ "@" ++ inet_db:gethostname() ++
                  "." ++ inet_db:res_option(domain);
         false ->
             SNode ++ "@" ++ inet_db:gethostname();
         _ ->
             SNode
         end
    end,
    list_to_atom(SNode1).

