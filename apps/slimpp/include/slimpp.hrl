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

-define(COPYRIGHT, "Copyright (c) 2014 SlimChat.IO"). 

-type oid_class() :: uid | vid | sid | gid | atom().

%example: {slim_oid, <<"domain">>, uid, <<"name">>}
%class: uid | vid | sid | gid
-record(slim_oid, {
	domain	:: binary(),
	class	:: oid_class(),
	name	:: binary()
}).

-type oid() :: #slim_oid{}.

-type content_type() :: text | image | audio | video | file | atom().
%example: {slim_content, <<"image">>, <<"base64">>, <<"aakkdka2rfka">>}
-record(slim_content, {
	type		:: content_type(),
	encoding	:: binary(),
	body		:: binary()
}).

-type content() :: #slim_content{}.

-record(slim_message, {
	id			:: binary(),
	from		:: oid(),
	nick		:: binary(),
	to			:: oid(),
	ts			:: float(),
	type = chat :: chat | grpchat,
	content		:: content() | binary()
}).

-type message() :: #slim_message{}.

-record(slim_presence, {
	type	:: atom(),
	from	:: oid(),
	nick	:: binary(),
	to		:: oid(),
	show	:: binary(),
	status	:: binary()
}).

-type presence() :: #slim_presence{}.

-record(slim_status, {
		from	:: oid(),
		nick	:: binary(),
		to		:: oid(),
		show	:: binary()}).

-record(slim_ticket, {
		class	:: atom(),
		name	:: binary(),
		token	:: binary()}).

-type ticket() :: #slim_ticket{}.

-record(slim_error, {
	code	:: integer(),
	reason	:: binary()
}).

%name: <<"a/b/c">>
%node: node()
%words: [<<"a">>, <<"b">>, <<"c">>]
-record(topic, {
		name	:: binary(),
		node	:: node()
}).

-type topic() :: #topic{}.

-record(topic_subscriber, {
		topic	:: binary(),
		subpid	:: pid()
}).

-record(topic_trie_node, {
		node_id			:: binary(),
		edge_count = 0	:: non_neg_integer(),
		topic			:: binary()
}).

-record(topic_trie_edge, {
		node_id	:: binary(),
		word	:: binary()
}).

-record(topic_trie, {
		edge	:: #topic_trie_edge{},
		node_id	:: binary()
}).


