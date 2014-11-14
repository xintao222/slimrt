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

%%TODO: FIXME Later

-module(slim_router).

-author('feng.lee@slimchat.io').

-export([start_link/0, 
		lookup/1, 
		register/1, 
		unregister/1, 
        route/3]).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

%%
%% @doc start router server.
%% 
-spec start_link() -> {ok, pid()}.
start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%
%% @doc Lookup route with oid
%%
-spec lookup(Oid :: oid()) -> list(#slimrt_route{}).
lookup(Oid) when is_record(Oid, slim_oid) ->
	mnesia:dirty_read({slimrt_route, Oid});

%%
%% @doc Lookup routes with oids
%%
lookup(Oids) when is_list(Oids) ->
	lists:flatten([lookup(Oid) || Oid <- Oids]).

%%
%% @doc Register route.
%%
-spec register(Route :: #slim_route{}) -> ok.
register(Route = #slim_route{oid=Oid, pid=Pid}) ->
	slim_pubsub:subscribe(slim_oid:topic(Oid), Pid),
	gen_server:call(?MODULE, {register, Route}).

%%
%% @doc Unregister route.
%%
-spec unregister(Oid :: oid()) -> any().
unregister(Oid) when is_record(Oid, slim_oid) ->
    Topic = slim_oid:topic(Oid),
    lists:foreach(fun(Route = #slim_route{pid=Pid}) ->
		slim_pubsub:unsubscribe(Topic, Pid),
		gen_server:call(?MODULE, {unregister, Route})
    end, lookup(Oid)).

%%
%% @doc route a packet.
%%
-spec route(From :: oid(), To :: oid(), Packet :: term()) -> any().
route(_From, To, Packet) ->
	slim_pubsub:publish(nextalk_oid:topic(To), Packet).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	process_flag(trap_exit, true),
	mnesia:create_table(slim_route,
		[{ram_copies, [node()]},
		 {attributes, record_info(fields, slim_route)},
		 {index, [mon]}]),
	mnesia:add_table_copy(slim_route, node(), ram_copies),
    {ok, state}.

handle_call(_Request, _From, State) ->
    Mon = erlang:monitor(process, Pid),
    mnesia:dirty_write(Route#slim_route{mon = Mon}),
    slim_meter:incr(route, nextalk_oid:domain(Oid)),
    {reply, ok, State};

handle_call({unregister, #slim_route{oid = Oid, mon = Mon}}, _From, State) ->
	erlang:demonitor(Mon),
	mnesia:dirty_delete({slim_route, Oid}),
	slim_meter:decr(route, nextalk_oid:domain(Oid)),
	folsom_metrics:notify('slim.routes', {dec, 1}),
	{reply, ok, State};

handle_call(Req, From, State) ->
	{stop, {badreq, From, Req}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Mon, _Type, _Object, _Info}, State) ->
	Routes = mnesia:dirty_index_read(slim_route, Mon, #nextalk_route.mon),
    [begin 
        mnesia:dirty_delete(slim_route, Oid),
        slim_meter:decr(route, nextalk_oid:domain(Oid)),
		folsom_metrics:notify('slim.routes', {dec, 1})
     end || #slim_route{oid = Oid} <- Routes],
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

