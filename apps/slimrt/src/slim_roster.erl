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

-module(slimrt_roster).

-include_lib("slimpp/include/slimpp.hrl").

-include("slimrt.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0
         buddies/1,
         add/2,
         remove/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
         terminate/2,
		 code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get buddies of an oid
-spec buddies(Oid :: oid()) -> list(#slim_roster{}).
buddies(Oid) when is_record(Oid, slim_oid) ->
    mnesia:dirty_read(slim_roster, Oid).

-spec add(Oid :: oid(), Buddies :: list(oid())) -> any().
add(Oid, Buddies) when is_record(Oid, slim_oid) and is_list(Buddies) ->
    mnesia:sync_dirty(fun() ->
        [begin 
            mnesia:write(#slim_roster{oid = Oid, fid = Buddy}),
            mnesia:write(#slim_roster{oid = Buddy, fid = Oid})
        end || Buddy <- Buddies]
    end).

-spec remove(Oid :: oid()) -> any().
remove(Oid) when is_record(Oid, slim_oid) ->
    mnesia:sync_dirty(fun() ->
        Oids = [Fid || #slim_roster{fid = Fid} <- mnesia:read(slim_roster, Oid)],
        [mnesia:delete({slim_roster, Id}) || Id <- [Oid|Oids]]
    end).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	mnesia:create_table(slim_roster,
		[{type, bag},
         {ram_copies, [node()]},
		 {attributes, record_info(fields, slim_roster)}]),
	mnesia:add_table_copy(slim_roster, node(), ram_copies),
    {ok, state}.

handle_call(Req, _From, State) ->
	Error = {badreq, Req},
    {stop, {error, Error}, State}.

handle_cast(Msg, State) ->
	Error = {badmsg, Msg},
	{stop, {error, Error}, State}.

handle_info(Info, State) ->
	Error = {badinfo, Info},
	{stop, {error, Error}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

