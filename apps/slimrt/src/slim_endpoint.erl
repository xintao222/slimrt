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

%%------------------------------------------------------------------------------
%%
%% Description:  Core Route Endpoint Module. 
%%
%%------------------------------------------------------------------------------

-module(slim_endpoint).

-author('feng.lee@slimchat.io').

-include_lib("slimpp/include/slimpp.hrl").

-include("slimrt.hrl").

-include("slim_log.hrl").

-import(erlang, [send_after/3]).

-export([start_link/1,
        clients/1,
		%%buddies/2,
		update/2, %% TODO:
        bind/2, unbind/2,
		subscribe/3, unsubscribe/3,
		publish/2, publish/3,
		send/3]).

-export([join/3, leave/3]).

-behavior(gen_server).

-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3]).

-record(state, {endpoint, idle_timer, tickets = []}).

-spec start_link({Endpoint	:: endpoint(), 
				  Buddies	:: list(oid()), 
				  Rooms		:: list(oid())}) -> 
	{ok, pid()} | ignore | {error, term()}.
start_link({Endpoint, Buddies, Rooms}) ->
    gen_server:start_link(?MODULE, {Endpoint, Buddies, Rooms}, []).

%%
%% @doc endpoint information.
%% 
-spec clients(EPid :: pid()) -> {ok, list(client())}.
clients(EPid) ->
    gen_server:call(EPid, clients).

%%FIXME Later
%-spec add_buddies(Pid :: pid(), Buddies :: list(oid())) -> ok.
%buddies(Pid, Buddies) ->
%	gen_server:call(Pid, {buddies, Buddies}).

%%
%% @doc update buddies, rooms, show...
%%
-spec update(EPid :: pid(), Data :: list()) -> ok.
update(EPid, Data) ->
    gen_server:call(EPid, {update, Data}).

%%
%% @doc bind a client to this endpoint, a client is a browser tab or window.
%%
-spec bind(EPid :: pid(), ClientId :: binary()) -> {ok, Ticket :: ticket()}.
bind(EPid, ClientId) ->
	gen_server:call(EPid, {bind, ClientId}).
    
%%
%% @doc a client subscribe to this endpoint.
%%
-spec subscribe(EPid 	:: pid(),
				Ticket 	:: ticket(),
				CPid 	:: pid()) -> ok.
subscribe(EPid, Ticket, CPid) when ?is_ticket(Ticket) ->
	gen_server:cast(EPid, {subscribe, Ticket, CPid}).

%%
%% @doc a client unsubscribe to this endpoint.
%%
-spec unsubscribe(EPid :: pid(), Ticket :: ticket(), CPid :: pid()) -> ok.
unsubscribe(EPid, Ticket, CPid) when ?is_ticket(Ticket) ->
	gen_server:cast(EPid, {unsubscribe, Ticket, CPid}).

%%
%% @doc a session unbind this endpoint, a browser tab or window is closed.
%%
-spec unbind(EPid :: pid(), Ticket :: ticket()) -> ok.
unbind(EPid, Ticket)  ->
	gen_server:call(EPid, {unbind, Ticket}).

%%
%% @doc send a message.
%%
-spec send(EPid :: pid(), Ticket :: ticket(), Message :: message()) -> ok.
send(EPid, Ticket, Message) when ?is_ticket(Ticket), ?is_message(Message) ->
	gen_server:cast(EPid, {message, Ticket, Message}).

%%
%% @doc publish presence.
%%
-spec publish(EPid :: pid(), Presence :: presence()) -> ok.
publish(EPid, Presence) when ?is_presence(Presence) ->
	gen_server:cast(EPid, {presence, Presence}).

%%
%% @doc publish presence with the client ticket.
%%
-spec publish(EPid :: pid(), Ticket :: ticket(), Presence :: presence()) -> ok.
publish(EPid, Ticket, Presence) when ?is_presence(Presence) ->
	gen_server:cast(EPid, {presence, Ticket, Presence}).


%%
%% @doc endpoint join a room
%%
-spec join(EPid :: pid(), Room:: oid(), Nick :: binary()) -> ok.
join(EPid, Room, Nick) when ?is_oid(Room) ->
	gen_server:call(EPid, {join, Room, Nick}).

%%
%% @doc endpoint leave a room
%%
-spec leave(EPid :: pid(), Room :: oid(), Nick :: binary()) -> ok.
leave(EPid, Room, Nick) when ?is_oid(Room) ->
	gen_server:call(EPid, {leave, Room, Nick}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init({Endpoint = #slim_endpoint{oid = EpOid}, Buddies, Rooms}) ->
    process_flag(trap_exit, true),
	add_buddies(EpOid, Buddies),
	join_rooms(Endpoint, Rooms),
	register_route(Endpoint),
	send_available(Endpoint),
	Ref = send_after(?IDLE_TIMEOUT, self(), idle_timeout),
    {ok, #state{endpoint = Endpoint, idle_timer = Ref}}.

add_buddies(EpOid, Buddies) ->
	%%TODO: only onlines?
    Onlines = [Oid || #slim_route{oid = Oid} <- slim_router:lookup(Buddies)],
	slim_roster:add(EpOid, Onlines).

join_rooms(#slim_endpoint{oid=Oid, nick=Nick}, Rooms) ->
	slim_grpchat:join(Rooms, Oid, self(), Nick).

register_route(#slim_endpoint{oid = Oid, show = Show}) ->
    slim_router:register(#slim_route{oid = Oid, pid = self(), show = Show}).

send_available(#slim_endpoint{oid = Oid, nick = Nick, show = Show, status = Status}) ->
	publish(self(), #slim_presence{
		type = online, 
		nick = Nick, 
		from = Oid, 
		show = Show, 
		status = Status
	}).

%%
        %TODO: should update rooms
%%       if
%%            Show == OldShow -> ignore;
%%            true -> slim_router:update(Route#slim_route{show=Show})
%%       end,


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%%ClientId is not used...
handle_call({bind, _ClientId}, _From, State = #state{endpoint = Endpoint, idle_timer = IdleTimer, tickets = Tickets}) ->
	#slim_endpoint{oid = Oid} = Endpoint,
	#slim_oid{domain = Domain, class = Cls, id = Id} = Oid,
    Ticket = slim_ticket:make(Cls, Id),
	?INFO("bind ~p to ~p", [Ticket, Oid]),
	cancel_timer(IdleTimer),
	undefined = get(Ticket),
	Ref = send_after(?IDLE_TIMEOUT, self(), {idle_timeout, Ticket}),
	Client = #slim_client{ticket = Ticket, ref = Ref},
	put(Ticket, Client),
    slim_meter:incr(bind, Domain),
	slim_cm:create(Ticket, self()),
    {reply, {ok, Ticket}, State#state{idle_timer = undefined, tickets = [Ticket|Tickets]}};

handle_call({unbind, Ticket}, _From,  State = #state{endpoint = #slim_endpoint{oid = Oid}, tickets = Tickets}) ->
	?INFO("unbind ~p from ~p", [Ticket, Oid]),
	NewState = 
	case get(Ticket) of
    Client when ?is_client(Client) ->
		%TODO: mon is undefined???
		try_demonitor(Client#slim_client.mon),
		cancel_timer(Client#slim_client.ref),
		case Client#slim_client.pid of
		undefined -> 
			ok;
		CPid ->
			case is_process_alive(CPid) of
			true -> CPid ! stop;
			false -> ok
			end
		end,
		erase(Ticket),
		Tickets1 = lists:delete(Ticket, Tickets),
		case length(Tickets1) of
		0 ->
			Ref = send_after(?IDLE_TIMEOUT, self(), idle_timeout),
			State#state{idle_timer = Ref, tickets = Tickets1};
		_ ->
			State#state{tickets = Tickets1}	
		end;
	undefined ->
        ?WARNING("unbind ticket is not existed: ~p", [Ticket]),
		State
	end,
	slim_meter:incr(unbind, Oid#slim_oid.domain),
	{reply, ok, NewState};

%%TODO: support later...
handle_call({update, Data}, From, State) ->
	%%TODO:
	%%...
	%%...
	%%...
    {reply, ok, State};

handle_call({join, Room, Nick}, _From, #state{endpoint = #slim_endpoint{oid = Oid}} = State) ->
	slim_grpchat:join(Room, Oid, self(), Nick),
	Presence = #slim_presence{
		type = join,
		to = Room,
		from = Oid,
		nick = Nick,
		show = <<"available">>},
	slim_router:route(Oid, Room, Presence), 
	{reply, ok, State};

handle_call({leave, Room, Nick}, _From, #state{endpoint = #slim_endpoint{oid = Oid}} = State) ->
	slim_grpchat:leave(Room, Oid, self()),
	Presence = #slim_presence{
		type = leave,
		from = Oid,
        to   = Room,
		nick = Nick,
		show = <<"available">>},
	slim_router:route(Oid, Room, Presence),
	{reply, ok, State};

handle_call(Req, _From, State) ->
    {stop, {badreq, Req}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({subscribe, Ticket, CPid}, State) ->
	case get(Ticket) of
	Client when is_record(Client, slim_client) ->
		?DEBUG("subscribe ~p, ~p", [Ticket, CPid]),
		cancel_timer(Client#slim_client.ref),
		%TODO: FIX ME UNDEFINED
		try_demonitor(Client#slim_client.mon),
		case Client#slim_client.pid of
		undefined -> 
			ok;
        CPid ->
            ?ERROR("assert failure: subscribed by the same pid:~p", [CPid]);
		OldCPid ->
			case is_process_alive(OldCPid) of
			true -> OldCPid ! stop;
			false -> ok
			end
		end,
        %get packets first
		Packets = Client#slim_client.packets,
        %monitor process
        Mon = erlang:monitor(process, CPid),
        NewClient = Client#slim_client{pid = CPid, ref = undefined, mon = Mon, packets = []},
        put(Ticket, NewClient),
		case length(Packets) of
		0 -> ok;
		_ -> CPid ! {ok, Packets}
		end;
	undefined ->
		?ERROR("illegal ticket: ~p", [Ticket]),
		CPid ! stop
	end,
	{noreply, State, hibernate};

handle_cast({unsubscribe, Ticket, CPid}, State) ->
    case get(Ticket) of
    Client when is_record(Client, slim_client)->
		if
		(Client#slim_client.pid == undefined) or (Client#slim_client.pid == CPid) ->
			?DEBUG("unsubscribe ~p ~p", [Ticket, CPid]),
			%TODO: FIX ME UNDEFINED
			try_demonitor(Client#slim_client.mon),
			cancel_timer(Client#slim_client.ref),
			Ref = send_after(?IDLE_TIMEOUT, self(), {idle_timeout, Ticket}),
            NewClient = Client#slim_client{pid = undefined, ref = Ref, mon = undefined},
			put(Ticket, NewClient);
        true ->
            ?ERROR("~p cannot not unsubscribe ~p", [CPid, Client#slim_client.pid])
        end;
    undefined ->
		?ERROR("unsubscribed ticket is not existed: ~p", [Ticket])
    end,
	{noreply, State};

handle_cast({message, FromTicket, Message = #slim_message{to = To}}, 
	State = #state{endpoint = Endpoint, tickets = Tickets}) ->

	Oid = Endpoint#slim_endpoint.oid,
    %% users ---- pid
    %% sync all subscribers of sender
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
						%TODO: fixme later, should be handled when unsubscribed?...
						%%FIXME Later: merge from nextalk 6....what's the fuck...
						if 
						Client#slim_client.type == poll -> %%Really right?? unsubscribe or 'DOWN' to handle this???
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
	end, Tickets),
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
	State = #state{endpoint = Endpoint}) ->
	Oid = Endpoint#slim_endpoint.oid,
    Presence1 = 
    if 
    (Type == show) and (Show == <<"invisible">>) ->
        Presence#slim_presence{type=offline, show = <<"unavailable">>};
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
		[Route] -> %TODO: fixme later... inconsistent changed
			slim_router:update(Route#slim_route{show=Show});
		[] ->
			?ERROR("route not found: ~p", [Oid])
		end,
		{noreply, State};
	true ->
		{noreply, State}
	end;

handle_cast({presence, Ticket, Presence}, State) ->
	%%TODO: broadcast other clients???
	handle_cast({presence, Presence}, State),
	{noreply, State};

handle_cast(Msg, State) ->
    {stop, {badmsg, Msg}, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({dispatch, Packet}, State = #state{endpoint = #slim_endpoint{oid = Oid}, tickets = Tickets}) ->
	?INFO("~p Got Packet: ~n~p", [Oid, Packet]),
	case Packet of
	%from group chat topic
	#slim_message{from = Oid, type = grpchat} ->
		ignore; %%group chat, message from self
	_ -> 
		dispatch(Packet, Tickets)
	end,
    {noreply, State};

handle_info({'DOWN', Mon, _Type, _Object, _Info}, #state{tickets = Tickets} = State) ->
	case find_client(Tickets, Mon) of
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

handle_info({idle_timeout, Ticket}, #state{tickets = Tickets} = State) ->
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
	Tickets1 = lists:delete(Ticket, Tickets),
	slim_cm:destroy(Ticket),
	case length(Tickets1) of
	0 ->
		Ref = send_after(?IDLE_TIMEOUT, self(), idle_timeout),
		%NOTICE: hibernate when no tickets?
		{noreply, State#state{idle_timer = Ref, tickets = []}, hibernate};
	_ ->
		{noreply, State#state{tickets = Tickets1}}
	end;

handle_info(idle_timeout, #state{tickets = Tickets} = State) ->
	case length(Tickets) of
	0 -> ok;
	I -> ?ERROR("endpoint idle_timeout when ~p clients left", [I])
	end,
	{stop, normal, State};

handle_info(stop, State) ->
	?INFO_MSG("endpoint received stop"),
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
terminate(_Reason, #state{endpoint = Endpoint}) ->
	Oid = Endpoint#slim_endpoint.oid,
	Presence = #slim_presence{
				type = offline, 
				from = Oid, 
				show = <<"unavailable">>},
	[slim_router:route(Oid, Buddy#slim_roster.fid, Presence) || 
		Buddy <- slim_roster:buddies(Oid)],
	slim_cm:destroy(self()),
	slim_roster:remove(Oid),
	slim_grpchat:leave(Oid, self()),
    slim_router:unregister(Oid),
    ?DEBUG("endpoint terminated: ~p", [slim_oid:topic(Oid)]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

dispatch(Packet, Tickets) ->
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
	end, Tickets).

find_client([], _Mon) ->
	false;

find_client([Ticket|Tickets], Mon) ->
	#slim_client{mon = M} = Client = get(Ticket),
	if 
	M == Mon -> {ok, Client};
	true -> find_client(Tickets, Mon)
	end.

try_demonitor(undefined) -> ok;
try_demonitor(Mon) -> erlang:demonitor(Mon).

cancel_timer(undefined) -> ok;
cancel_timer(Ref) -> erlang:cancel_timer(Ref).


