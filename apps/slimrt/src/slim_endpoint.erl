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

-module(slim_endpoint).

-author('feng.lee@slimchat.io').

-include_lib("slimpp/include/slimpp.hrl").

-include("slimrt.hrl").

-include("slim_log.hrl").

-import(erlang, [send_after/3]).

-export([start_link/1,
        info/1,
		buddies/2,
        bind/2, 
		unbind/2, 
		subscribe/3, 
		unsubscribe/3, 
		send/4, 
		send/2,
        update/2]).

%TODO: Group operation
-export([join/3,
		leave/3]).

-behavior(gen_server).

-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3]).

-record(state, {oid, endpoint, ref, buddies, rooms, clients = []}).

%online
%offline
%subscribe
%unsubscribe
%TODO: THIS FILE SHOULD BE REWRITE LATER.
-spec start_link({Endpoint :: #slim_endpoint{}, 
				  Buddies :: list(), 
				  Rooms :: list()}) -> 
	{ok, pid()} | ignore | {error, term()}.
start_link({Endpoint, Buddies, Rooms}) ->
    gen_server:start_link(?MODULE, [{Endpoint, Buddies, Rooms}], []).

%%
%% @doc endpoint information.
%% 
-spec info(Pid :: pid()) -> {ok, #slim_endpoint{}}.
info(Pid) ->
    gen_server:call(Pid, info).

%%FIXME Later
-spec add_buddies(Pid :: pid(), Buddies :: list(oid())) -> ok.
buddies(Pid, Buddies) ->
	gen_server:call(Pid, {buddies, Buddies}).

%%
%% @doc bind a session to this endpoint, a session is a browser tab or window.
%%
-spec bind(Pid :: pid(), Ticket :: ticket()) -> ok.
bind(Pid, Ticket) when is_record(Ticket, slim_ticket)->
	gen_server:call(Pid, {bind, Ticket}).
    
%%
%% @doc a session subscribe to this endpoint.
%%
-spec subscribe(Pid 	:: pid(), 
				Ticket 	:: ticket(),
				SPid 	:: pid()) -> ok. 
subscribe(Pid, Ticket, SPid) when is_record(Ticket, slim_ticket) ->
	gen_server:cast(Pid, {subscribe, Ticket, SPid}).

%%
%% @doc a session unsubscribe to this endpoint.
%%
-spec unsubscribe(Pid :: pid(), Ticket :: ticket(), SPid :: pid()) -> ok.
unsubscribe(Pid, Ticket, SPid) ->
	gen_server:cast(Pid, {unsubscribe, Ticket, SPid}).

%%
%% @doc a session unbind this endpoint, a browser tab or window is closed.
%%
-spec unbind(Pid :: pid(), Ticket :: ticket()) -> ok.
unbind(Pid, Ticket)  ->
	gen_server:call(Pid, {unbind, Ticket}).

%%
%% @doc send a message.
%%
-spec send(Pid :: pid(), Ticket :: ticket(), To :: oid(), 
		Packet :: message()) -> ok.
send(Pid, Ticket, To, Message) when is_record(Message, slim_message) ->
	gen_server:cast(Pid, {message, Ticket, To, Message}).

%%
%% @doc send presence.
%%
-spec send(Pid :: pid(), Presence :: #slim_presence{}) -> ok.
send(Pid, Presence) when is_record(Presence, slim_presence) ->
	gen_server:cast(Pid, {presence, Presence}).

-spec update(Pid :: pid(), Data :: tuple()) -> ok.
update(Pid, Data) ->
    gen_server:cast(Pid, {update, Data}).

%%
%% @doc endpoint join a room
%%
-spec join(Pid :: pid(), Gid :: oid(), Nick :: binary()) -> ok.
join(Pid, Gid, Nick) ->
	gen_server:call(Pid, {join, Gid, Nick}).

%%
%% @doc endpoint leave a room
%%
-spec leave(Pid :: pid(), Gid :: oid(), Nick :: binary()) -> ok.
leave(Pid, Gid, Nick) ->
	gen_server:call(Pid, {leave, Gid, Nick}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([{#slim_endpoint{oid = Oid} = Endpoint, Buddies, Rooms}]) ->
    process_flag(trap_exit, true),
	add_buddies(Endpoint, Buddies),
	join_rooms(Endpoint, Rooms),
	register_route(Endpoint),
	send_available(Endpoint),
    rooms_online(Endpoint, Rooms),
	Ref = send_after(?IDLE_TIMEOUT, self(), idle_timeout),
    {ok, #state{endpoint = Endpoint, ref = Ref, buddies = Buddies, rooms = Rooms}}.

add_buddies(#slim_endpoint{oid = ThisOid}, Buddies) ->
    Onlines = [Oid || #slim_route{oid = Oid}
					<- slim_router:lookup(Buddies)],
	slim_roster:add(ThisOid, Onlines).

join_rooms(#slim_endpoint{oid = Oid, nick = Nick}, Rooms) ->
	slim_grpchat:join(Rooms, Oid, self(), Nick).

register_route(#slim_endpoint{oid = Oid, show=Show}) ->
	Route = #slim_route{oid = Oid, pid = self(), show=Show},
    slim_router:register(Route).

send_available(#slim_endpoint{oid = Oid, nick = Nick, show = Show, status = Status}) ->
    Presence = #slim_presence{type = online, nick = Nick, 
        from = Oid, show = Show, status = Status},
	send(self(), Presence).

rooms_online(#slim_endpoint{oid = Oid, nick=Nick}, Rooms) ->
    Presence = fun(Room) -> 
	    #slim_presence{
            type = grponline,
            to   = Room,
            from = Oid,
            nick = Nick,
            show = <<"available">>,
            status = slim_oid:name(Room)}
    end,
    [slim_router:route(Oid, Room, Presence(Room)) || Room <- Rooms].

rooms_offline(#slim_endpoint{oid = Oid, nick=Nick}, Rooms) -> 
    Presence = fun(Room) -> 
	    #slim_presence{
            type = grpoffline,
            to   = Room,
            from = Oid,
            nick = Nick,
            show = <<"unavailable">>,
            status = slim_oid:name(Room)}
    end,
    [slim_router:route(Oid, Room, Presence(Room)) || Room <- Rooms].

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(info, _From, State) ->
    {reply, {ok, State}, State};

handle_call({bind, Ticket}, _From, #state{endpoint = #slim_endpoint{oid = Oid}, ref = IdleTimer, clients = Clients} = State) ->
	?INFO("bind: ~p", [Ticket]),
	cancel_timer(IdleTimer),
	undefined = get(Ticket),
	Ref = send_after(?IDLE_TIMEOUT, self(), {idle_timeout, Ticket}),
	Client = #slim_client{ticket = Ticket, ref = Ref},
	put(Ticket, Client),
    slim_meter:incr(bind, Oid#slim_oid.domain),
    {reply, ok, State#state{ref = undefined, clients = [Ticket|Clients]}};

handle_call({unbind, Ticket}, _From, #state{oid = Oid, clients = Clients} = State) ->
	?INFO("unbind: ~p", [Ticket]),
	NewState = 
	case get(Ticket) of
    Client when ?is_client(Client) ->
		%TODO: mon is undefined???
		try_demonitor(Client#slim_client.mon),
		cancel_timer(Client#slim_client.ref),
		case Client#slim_client.pid of
		undefined -> 
			ok;
		SPid ->
			case is_process_alive(SPid) of
			true -> SPid ! stop;
			false -> ok
			end
		end,
		erase(Ticket),
		Clients1 = lists:delete(Ticket, Clients),
		case length(Clients1) of
		0 ->
			Ref = send_after(?IDLE_TIMEOUT, self(), idle_timeout),
			State#state{ref = Ref, clients = Clients1};
		_ ->
			State#state{clients = Clients1}	
		end;
	undefined ->
        ?WARNING("unbind ticket is not existed: ~p", [Ticket]),
		State
	end,
	slim_meter:incr(unbind, Oid#slim_oid.domain),
	{reply, ok, NewState};


handle_call({join, Gid, Nick}, _From, State = #state{endpoint = Endpoint}) ->
	Oid = Endpoint#slim_endpoint.oid,
	slim_grpchat:join(Gid, Oid, self(), Nick),
	Presence = #slim_presence{
                                type = join,
                                to = Gid,
								from = Oid,
								nick = Nick,
								show = <<"available">>,
								status = slim_oid:name(Gid)},
	slim_router:route(Oid, Gid, Presence), 
	{reply, {ok, slim_grpchat:members(Gid)}, State};

handle_call({leave, Gid, Nick}, _From, State = #state{endpoint=Endpoint}) ->
	Oid = Endpoint#slim_endpoint.oid,
	slim_grpchat:leave(Gid, Oid, self()),
	Presence = #slim_presence{type = leave,
								from = Oid,
                                to   = Gid,
								nick = Nick,
								show = <<"available">>,
								status = slim_oid:name(Gid)},
	slim_router:route(Oid, Gid, Presence),
	{reply, ok, State};

handle_call(Req, _From, State) ->
    {stop, {badreq, Req}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({update, {buddies, Buddies}}, #state{endpoint=Endpoint} = State) ->
    add_buddies(Endpoint, Buddies),
    {noreply, State#state{buddies = Buddies}};

handle_cast({update, {rooms, Rooms}}, #state{endpoint=Endpoint, rooms=OldRooms} = State) ->
    join_rooms(Endpoint, Rooms),
    rooms_online(Endpoint, Rooms -- OldRooms),
    {noreply, State#state{rooms = Rooms}};

handle_cast({subscribe, Type, Ticket, SPid}, State) ->
	case get(Ticket) of
	Client when is_record(Client, slim_client) ->
		?INFO("subscribe ~p, ~p", [Ticket, SPid]),
		cancel_timer(Client#slim_client.ref),
		%TODO: FIX ME UNDEFINED
		try_demonitor(Client#slim_client.mon),
		case Client#slim_client.pid of
		undefined -> 
			ok;
        SPid ->
            ?ERROR("assert failure: subscribed by the same pid:~p", [SPid]);
		OldSPid ->
			case is_process_alive(OldSPid) of
			true -> OldSPid ! stop;
			false -> ok
			end
		end,
        %get packets first
		Packets = Client#slim_client.packets,
        %monitor process
        Mon = erlang:monitor(process, SPid),
        NewClient = Client#slim_client{pid = SPid, ref = undefined, type = Type, mon = Mon, packets = []},
        put(Ticket, NewClient),
		case length(Packets) of
		0 -> ok;
		_ -> SPid ! {ok, Packets}
		end;
	undefined ->
		?ERROR("illegal client: ~p", [Ticket]),
		SPid ! stop
	end,
	{noreply, State, hibernate};

handle_cast({unsubscribe, Ticket, SPid}, State) ->
    case get(Ticket) of
    Client when is_record(Client, slim_client)->
		if
		(Client#slim_client.pid == undefined) or (Client#slim_client.pid == SPid) ->
			?INFO("unsubscribe ~p ~p", [Ticket, SPid]),
			%TODO: FIX ME UNDEFINED
			try_demonitor(Client#slim_client.mon),
			cancel_timer(Client#slim_client.ref),
			Ref = send_after(?IDLE_TIMEOUT, self(), {idle_timeout, Ticket}),
            NewClient = Client#slim_client{pid = undefined, ref = Ref, mon = undefined},
			put(Ticket, NewClient);
        true ->
            ?ERROR("~p cannot not unsubscribe ~p", [SPid, Client#slim_client.pid])
        end;
    undefined ->
		?ERROR("unsubscribed ticket is not existed: ~p", [Ticket])
    end,
	{noreply, State};

handle_cast({message, FromTicket, To, Message}, #state{oid = Oid, 
    clients = Clients} = State) ->
    %% users ---- pid
    %% sync all browsers of sender
	Message1 = Message#slim_message{from = Oid},
	lists:foreach(fun(Ticket) -> 
		case FromTicket == Ticket of
		true ->
			pass;
		false ->
			case get(Ticket) of
			Client when is_record(Client, slim_client)->
				Packets = Client#slim_client.packets,
				case Client#slim_client.pid of
					undefined ->
						put(Ticket, Client#slim_client{packets = [Message1|Packets]});
					Pid ->
						Pid ! {ok, [Message1|Packets]},
						%TODO: fixme later, should be handled when unsubscribed?
						if 
						Client#slim_client.type == poll ->
							erlang:demonitor(Client#slim_client.mon),
							put(Ticket, Client#slim_client{pid=undefined, mon=undefined, packets=[]});
						Client#slim_client.type == conn ->
							put(Ticket, Client#slim_client{packets=[]});
						true ->
							ignore
						end
				end;
			undefined ->
				?ERROR("no subscriber found: ~p", [Ticket])
			end
		end
	end, Clients),
	%%------------------------------------------------------------------------------------
    %% Send Message 
    %% slim_router:route -> slim_endpoint:dispatch =========
    %%                                                     ||
    %%                                                     \/
    %% ReceiverEndpoint <---LongPoll---> slim_jsonp  <- slim_endpoint:handle_info(packet)
	%%------------------------------------------------------------------------------------
	%% 
	slim_router:route(Oid, To, Message1),
	slim_meter:incr(message, Oid#slim_oid.domain),
	{noreply, State};

handle_cast({presence, Presence = #slim_presence{type=Type, nick=Nick, show=Show, status=Status}}, 
	#state{oid = Oid, endpoint=Endpoint, rooms=Rooms} = State) ->

    Presence1 = 
    if 
    (Type == show) and (Show == invisible) ->
        rooms_offline(Endpoint, Rooms),
        Presence#slim_presence{type=offline, show=unavailable};
    true ->
        Presence
    end,

	[slim_router:route(Oid, Buddy#slim_roster.fid, Presence1)
        || Buddy <- slim_roster:buddies(Oid)],

	if
	Type == show ->
		%%update presence.
		case slim_router:lookup(Oid) of
		[#slim_route{show=Show}] -> %not changed
			ignore;
		[Route] -> %changed
			slim_router:update(Route#slim_route{show=Show});
		[] ->
			?ERROR("updating show: ~p, route not found: ~p", [Show, Oid])
		end,
		{noreply, State#state{endpoint = Endpoint#slim_endpoint{nick=Nick, show=Show, status=Status}}};
	true ->
		{noreply, State}
	end;

%
%handle_cast({update, Props}, State = #state{endpoint = #slim_endpoint{show = Show}}) ->
%    case proplists:get_value(show, Props, Show) of
%    Show -> ignore;
%    Show1 -> 
%    end
%    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {badmsg, Msg}, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({dispatch, Packet}, State = #state{
	endpoint = #slim_endpoint{ oid = Oid }, clients = Clients}) ->
	?INFO("~p Got Packet: ~n~p", [Oid, Packet]),
	case Packet of	
	%from group chat topic
	#slim_message{from = Oid, type = grpchat} ->
		ignore;
	_ -> 
		dispatch(Packet, Clients)
	end,
    {noreply, State};

handle_info({'DOWN', Mon, _Type, _Object, _Info}, #state{clients = Clients} = State) ->
	case find_client(Clients, Mon) of
	{ok, Client = #slim_client{ticket = Ticket}} ->
	   %?INFO("down: ~p, ~p",[Ticket, Mon]),
       Ref1 = 
       case Client#slim_client.ref of
       undefined -> send_after(?IDLE_TIMEOUT, self(), {idle_timeout, Ticket});
       Ref -> Ref
       end,
       NewClient = Client#slim_client{pid = undefined, ref = Ref1, mon = undefined},
       put(Ticket, NewClient);
	false ->
		%?ERROR("cannot find down session: ~p", [Mon]),
		ok
	end,
	{noreply, State};

handle_info({idle_timeout, Ticket}, #state{clients = Clients} = State) ->
	?INFO("idle_timeout: ~p", [Ticket]),
    case get(Ticket) of
    undefined ->
        ?ERROR("assert failure: cannot idle_timeout ticket: ~p", [Ticket]);
    Client ->
        %debug
        if
        Client#slim_client.mon =/= undefined ->
            ?ERROR("assert failure, subscriber mon: ~p", [Client#slim_client.mon]);
        true ->
            ok
        end,
        %debug
        if
        Client#slim_client.pid =/= undefined ->
            ?ERROR("assert failure, subscriber spid: ~p", [Client#slim_client.pid]);
        true ->
            ok
        end
    end,
	erase(Ticket),
	Clients1 = lists:delete(Ticket, Clients),
	NewState = 
	case length(Clients1) of
	0 ->
		Ref = send_after(?IDLE_TIMEOUT, self(), idle_timeout),
		State#state{ref = Ref, clients = Clients1};
	_ ->
		State#state{clients = Clients1}	
	end,
	{noreply, NewState};

handle_info(idle_timeout, #state{clients = Clients} = State) ->
	case length(Clients) of
	0 -> ok;
	I -> ?ERROR("idle_timeout when ~p clients left", [I])
	end,
	{stop, normal, State};

handle_info(stop, State) ->
	?INFO("endpoint received stop info", []),
	{stop, normal, State};

handle_info(Info, State) ->
    ?ERROR("badinfo: ~p", [Info]),
    {stop, {error, badinfo}, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{oid = Oid, endpoint = Endpoint, rooms = Rooms}) ->
	Presence = #slim_presence{type = <<"offline">>, 
						 from = Oid, 
						 nick = Endpoint#slim_endpoint.nick, 
					     show = <<"unavailable">>, 
						 status = Endpoint#slim_endpoint.status},
	[slim_router:route(Oid, Buddy#slim_roster.fid, Presence) || 
		Buddy <- slim_roster:buddies(Oid)],
	slim_roster:remove(Oid),
	slim_grpchat:leave(Oid, self()),
    rooms_offline(Endpoint, Rooms),
    slim_router:unregister(Oid),
    ?INFO("endpoint is terminated: ~p",[Oid]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

dispatch(Packet, Clients) ->
    %% send Message to all the browsers of the receiver.
    %% slim_endpoint:dispatch -> slim_endpoint:handle_info(packet)
    %%                                              ||
    %%                                              ||
    %% Receiver Endpoint <---LongPoll---> slim_jsonp <--
	lists:foreach(fun(Ticket) -> 
		case get(Ticket) of
		Client when is_record(Client, slim_client) ->
			Packets = Client#slim_client.packets,
			case Client#slim_client.pid of
			undefined ->
				put(Ticket, Client#slim_client{packets = [Packet|Packets]});
			Pid ->
				Pid ! {ok, [Packet|Packets]},
				%TODO: fixme later, should be handled when unsubscribed?
				if 
				Client#slim_client.type == poll ->
					erlang:demonitor(Client#slim_client.mon),
					put(Ticket, Client#slim_client{pid=undefined, mon=undefined, packets=[]});
				Client#slim_client.type == conn ->
					put(Ticket, Client#slim_client{packets=[]});
				true ->
					ignore
				end
			end;
		undefined ->
			?ERROR("undefined subscriber in dict: ~p", [Ticket])
		end
	end, Clients).

find_client([], _Mon) ->
	false;

find_client([Ticket|Clients], Mon) ->
	#slim_client{mon = M} = Client = get(Ticket),
	if 
	M == Mon -> {ok, Client};
	true -> find_client(Clients, Mon)
	end.

try_demonitor(undefined) -> ok;
try_demonitor(Mon) -> erlang:demonitor(Mon).

cancel_timer(undefined) -> ok;
cancel_timer(Ref) -> erlang:cancel_timer(Ref).


