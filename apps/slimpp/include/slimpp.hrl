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

-define(record_to_list(Tag, Val), lists:zip(record_info(fields, Tag), tl(tuple_to_list(Val)))).

%%------------------------------------------------------------------------------
% slim oid
%%------------------------------------------------------------------------------
-type oid_class() :: uid | vid | sid | gid | atom().

%example: {slim_oid, <<"domain">>, uid, <<"name">>}
-record(slim_oid, {
	domain	:: binary(),
	class	:: oid_class(),
	name	:: binary()
}).

-type oid() :: #slim_oid{}.

%%------------------------------------------------------------------------------
%% slim message
%%------------------------------------------------------------------------------
-type message_type() :: chat | grpchat | event | atom().

-type message_format() :: text | image | audio | video | file | atom().

-record(slim_message, {
	%% message properties
	id			:: binary(), 		% message unique id
	chatid		:: binary(), 		% conversation id
	type		:: message_type(),  % content type 
	from		:: oid(), 	 		% send from
	nick		:: binary(), 		% sender nickname
	to			:: oid(),	 		% send to
	%% message content
	format		:: message_format(),% content format
	encoding	:: binary(),		% content encoding
	subject		:: binary(), 		% content subject/topic
	body		:: binary(),		% content body
	%% timestamp
	ts			:: integer()			%message timstamp
}).

-type message() :: #slim_message{}.

%%------------------------------------------------------------------------------
%% slim presence
%%------------------------------------------------------------------------------
-type presence_type() :: online | offline | show | subscribe | subscribed | unsubscribe | unsubscribed | hidden.
-type presence_show() :: available | unavailable | chat | away | busy | invisible | atom().

-record(slim_presence, {
	type	:: presence_type(),
	from	:: oid(),
	nick	:: binary(),
	to		:: oid(),		%% optional
	priority:: integer(),   %not used
	show	:: presence_show(),
	status	:: binary(),
	ts		:: integer()
}).

-type presence() :: #slim_presence{}.

%%------------------------------------------------------------------------------
%% slim ticket
%%------------------------------------------------------------------------------
-record(slim_ticket, {
	class	:: atom(),
	name	:: binary(),
	token	:: binary()
}).

-type ticket() :: #slim_ticket{}.

%%------------------------------------------------------------------------------
%% slim error
%%------------------------------------------------------------------------------
-record(slim_error, {
	code	:: integer(),
	reason	:: binary()
}).

%%------------------------------------------------------------------------------
%% pubsub topic
%%------------------------------------------------------------------------------
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


