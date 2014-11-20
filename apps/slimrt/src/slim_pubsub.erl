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

-module(slim_pubsub).

-include_lib("stdlib/include/qlc.hrl").

-include_lib("slimpp/include/slimpp.hrl").

-include("slimrt.hrl").

-include("slim_log.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([topics/0,
		subscribe/2,
		unsubscribe/2,
		publish/2,
		dispatch/2, %local node
		match/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
        terminate/2,
		code_change/3]).

-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%
%% @doc Start Pubsub.
%%
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%
%% @doc All topics
%%
-spec topics() -> list(topic()).
topics() ->
	mnesia:dirty_all_keys(topic).

%%
%% @doc Subscribe Topic
%%
-spec subscribe(Topic :: binary(), SubPid :: pid()) -> any().
subscribe(Topic, SubPid) when is_binary(Topic) and is_pid(SubPid) ->
	gen_server:call(?MODULE, {subscribe, Topic, SubPid}).

%%
%% @doc Unsubscribe Topic
%%
-spec unsubscribe(Topic :: binary(), SubPid :: pid()) -> any().
unsubscribe(Topic, SubPid) when is_binary(Topic) and is_pid(SubPid) ->
	gen_server:cast(?MODULE, {unsubscribe, Topic, SubPid}).

%%
%% @doc Publish to cluster node.
%%
-spec publish(Topic :: binary(), Packet :: term()) -> any().
publish(Topic, Packet) when is_binary(Topic) ->
	lists:foreach(fun(#topic{name=Name, node=Node}) ->
		case Node == node() of
		true -> dispatch(Name, Packet);
		false -> rpc:call(Node, ?MODULE, dispatch, [Name, Packet])
		end
	end, match(Topic)).

%dispatch locally, should only be called by publish
dispatch(Topic, Packet) when is_binary(Topic) ->
	[SubPid ! {dispatch, Packet} || #topic_subscriber{subpid=SubPid} <- ets:lookup(topic_subscriber, Topic)].

match(Topic) when is_binary(Topic) ->
	TrieNodes = mnesia:async_dirty(fun trie_match/1, [slim_topic:words(Topic)]),
    Names = [Name || #topic_trie_node{topic=Name} <- TrieNodes, Name=/= undefined],
	lists:flatten([mnesia:dirty_read(topic, Name) || Name <- Names]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
	mnesia:create_table(topic_trie, [
		{ram_copies, [node()]},
		{attributes, record_info(fields, topic_trie)}]),
	mnesia:add_table_copy(topic_trie, node(), ram_copies),
	mnesia:create_table(topic_trie_node, [
		{ram_copies, [node()]},
		{attributes, record_info(fields, topic_trie_node)}]),
	mnesia:add_table_copy(topic_trie_node, node(), ram_copies),
	mnesia:create_table(topic, [
		{type, bag},
		{record_name, topic},
		{ram_copies, [node()]}, 
		{attributes, record_info(fields, topic)}]),
	mnesia:add_table_copy(topic, node(), ram_copies),
	%TODO: 
	ets:new(topic_subscriber, [bag, named_table, {keypos, 2}]),
	{ok, #state{}}.

handle_call({subscribe, Topic, SubPid}, _From, State) ->
	case mnesia:transaction(fun trie_add/1, [Topic]) of
	{atomic, _} ->	
		case get({subscriber, SubPid}) of
		undefined -> 
			MonRef = erlang:monitor(process, SubPid),
			put({subcriber, SubPid}, MonRef),
			put({submon, MonRef}, SubPid);
		_ ->
			already_monitored
		end,
		ets:insert(topic_subscriber, #topic_subscriber{topic=Topic, subpid=SubPid}),
		{reply, ok, State};
	{aborted, Reason} ->
		{reply, {error, Reason}, State}
	end;

handle_call(Req, _From, State) ->
	{stop, {badreq, Req}, State}.

handle_cast({unsubscribe, Topic, SubPid}, State) ->
	%?INFO("unsubscribe: ~s", [Topic]),
	ets:delete_object(topic_subscriber, #topic_subscriber{topic=Topic, subpid=SubPid}),
	try_remove_topic(Topic),
	{noreply, State};

handle_cast(Msg, State) ->
	{stop, {badmsg, Msg}, State}.

handle_info({'DOWN', Mon, _Type, _Object, _Info}, State) ->
	case get({submon, Mon}) of
	undefined ->
		?ERROR("unexpected 'DOWN': ~p", [Mon]);
	SubPid ->
		%?INFO("subscriber DOWN: ~p", [SubPid]),
		erase({submon, Mon}),
		erase({subscriber, SubPid}),
		Subs = ets:match_object(topic_subscriber, #topic_subscriber{subpid=SubPid, _='_'}),
		[ets:delete_object(topic_subscriber, Sub) || Sub <- Subs],
		[try_remove_topic(Topic) || #topic_subscriber{topic=Topic} <- Subs]
	end,
	{noreply, State};

handle_info(Info, State) ->
	{stop, {badinfo, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
try_remove_topic(Name) when is_binary(Name) ->
	?INFO("try_remove_topic: ~s", [Name]),
	case ets:member(topic_subscriber, Name) of
	false -> 
		Topic = slim_topic:new(Name),
		?INFO("do remove: ~p", [Topic]),
		Fun = fun() -> 
			mnesia:delete_object(Topic),
			case mnesia:read(topic, Name) of
			[] -> trie_delete(Name);		
			_ -> ignore
			end
		end,
		Res = mnesia:transaction(Fun),
		?INFO("~p", [Res]);
	true -> 
		ok
	end.

trie_add(Topic) when is_binary(Topic) ->
	mnesia:write(slim_topic:new(Topic)),
	case mnesia:read(topic_trie_node, Topic) of
	[TrieNode=#topic_trie_node{topic=undefined}] ->
		mnesia:write(TrieNode#topic_trie_node{topic=Topic});
	[#topic_trie_node{topic=Topic}] ->
		ignore;
	[] ->
		%add trie path
		[trie_add_path(Triple) || Triple <- slim_topic:triples(Topic)],
		%add last node
		mnesia:write(#topic_trie_node{node_id=Topic, topic=Topic})
	end.

trie_delete(Topic) when is_binary(Topic) ->
	case mnesia:read(topic_trie_node, Topic) of
	[#topic_trie_node{edge_count=0}] -> 
		mnesia:delete({topic_trie_node, Topic}),
		trie_delete_path(lists:reverse(slim_topic:triples(Topic)));
	[TrieNode] ->
		mnesia:write(TrieNode#topic_trie_node{topic=Topic});
	[] ->
		ignore
	end.
	
trie_match(Words) ->
	trie_match(root, Words, []).

trie_match(NodeId, [], ResAcc) ->
	mnesia:read(topic_trie_node, NodeId) ++ 'trie_match_#'(NodeId, ResAcc);

trie_match(NodeId, [W|Words], ResAcc) ->
	lists:foldl(fun(WArg, Acc) ->
		case mnesia:read(topic_trie, #topic_trie_edge{node_id=NodeId, word=WArg}) of
		[#topic_trie{node_id=ChildId}] -> trie_match(ChildId, Words, Acc);
		[] -> Acc
		end
	end, 'trie_match_#'(NodeId, ResAcc), [W, "+"]).

'trie_match_#'(NodeId, ResAcc) ->
	case mnesia:read(topic_trie, #topic_trie_edge{node_id=NodeId, word="#"}) of
	[#topic_trie{node_id=ChildId}] ->
		mnesia:read(topic_trie_node, ChildId) ++ ResAcc;	
	[] ->
		ResAcc
	end.

trie_add_path({Node, Word, Child}) ->
	Edge = #topic_trie_edge{node_id=Node, word=Word},
	case mnesia:read(topic_trie_node, Node) of
	[TrieNode = #topic_trie_node{edge_count=Count}] ->
		case mnesia:read(topic_trie, Edge) of
		[] -> 
			mnesia:write(TrieNode#topic_trie_node{edge_count=Count+1}),
			mnesia:write(#topic_trie{edge=Edge, node_id=Child});
		[_] -> 
			ok
		end;
	[] ->
		mnesia:write(#topic_trie_node{node_id=Node, edge_count=1}),
		mnesia:write(#topic_trie{edge=Edge, node_id=Child})
	end.

trie_delete_path([]) ->
	ok;
trie_delete_path([{NodeId, Word, _} | RestPath]) ->
	Edge = #topic_trie_edge{node_id=NodeId, word=Word},
	mnesia:delete({topic_trie, Edge}),
	case mnesia:read(topic_trie_node, NodeId) of
	[#topic_trie_node{edge_count=1, topic=undefined}] -> 
		mnesia:delete({topic_trie_node, NodeId}),
		trie_delete_path(RestPath);
	[TrieNode=#topic_trie_node{edge_count=1, topic=_}] -> 
		mnesia:write(TrieNode#topic_trie_node{edge_count=0});
	[TrieNode=#topic_trie_node{edge_count=C}] ->
		mnesia:write(TrieNode#topic_trie_node{edge_count=C-1});
	[] ->
		throw({notfound, NodeId}) 
	end.

