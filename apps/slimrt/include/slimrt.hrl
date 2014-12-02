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

-define(APIVSN, "v1").  

-define(IDLE_TIMEOUT, 10000).

-define(POLL_TIMEOUT, 27000).

%%------------------------------------------------------------------------------
%% Client
%%------------------------------------------------------------------------------
%each client has a ticket :)
-record(slim_client, {
    ticket		:: ticket(),
	cid			:: binary(), %Not used: client id
    cname		:: binary(), %Not used: client name
    pid			:: pid(),
    ref			:: reference(),
    mon			:: reference(),
    type		:: atom(),
    packets=[]	:: list()
}).

-type client() :: #slim_client{}.

-define(is_client(Record), is_record(Client, slim_client)).

%%------------------------------------------------------------------------------
%% Endpoint
%%------------------------------------------------------------------------------
-record(slim_endpoint, {
    oid		:: oid(), 
    name	:: binary(),
    nick	:: binary(),
    domain	:: binary(),
	status	:: binary(),
    show = available	:: presence_show()
}).

-type endpoint() :: #slim_endpoint{}.

-define(is_endpoint(Record), is_record(Record, slim_endpoint)).

%%------------------------------------------------------------------------------
%% Roster
%%------------------------------------------------------------------------------
-record(slim_roster, {
    oid		:: oid(),
    fid		:: oid()
}).

-type roster() :: #slim_roster{}.

-define(is_roster(Record), is_record(Record, slim_roster)).

%%------------------------------------------------------------------------------
%% Room, FIXME Later
%%------------------------------------------------------------------------------
-record(slim_room, {
	gid		:: oid(), %room oid
	oid		:: oid(), % member oid
	nick	:: binary()
}).

-type room() :: #slim_room{}.

-define(is_room(Record), is_record(Record, slim_room)).

%%------------------------------------------------------------------------------
%% Route
%%------------------------------------------------------------------------------
-record(slim_route, {
	oid		:: oid(),
	pid		:: pid(),
	mon		:: reference(),
	show	:: presence_show()
}).

-type route() :: #slim_route{}.

-define(is_route(Record), is_record(Record, slim_route)).


