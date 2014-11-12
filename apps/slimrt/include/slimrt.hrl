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

-define(IDLE_TIMEOUT, 10000).

-define(POLL_TIMEOUT, 27000).

%each subscriber has a ticket:)
-record(slim_subscriber, {
    ticket		:: ticket(),
    spid		:: pid(),
    ref			:: reference(),
    mon			:: reference(),
    type		:: atom(),
    packets=[]	:: list()
}).

%show: ['available', 'away', 'chat', 'dnd', 'invisible', 'unavailable']
-record(slim_endpoint, {
    oid		:: oid(), 
    name	:: binary(),
    nick	:: binary(),
    domain	:: binary(),
    show = available	:: available | away | chat | dnd | invisible | unavailable,
    status = <<>>		:: binary()
}).

-record(slim_roster, {
    oid		:: oid(),
    fid		:: oid()
}).

%gid is a oid
-record(slim_room, {
		gid		:: oid(), %room oid
		oid		:: oid(), % member oid
		nick	:: binary()}).

-record(slim_route, {
		oid		:: oid(),
		pid		:: pid(),
		show	:: binary(),
		mon		:: reference()
}).

