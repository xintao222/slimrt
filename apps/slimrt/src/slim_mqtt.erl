%%------------------------------------------------------------------------------
%% Copyright (c) 2014, Feng Lee <feng@slimchat.io>
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

-module(slim_mqtt).

-author('feng@gmail.com').

-include_lib("slimpp/include/slimpp.hrl").

-include("slim_log.hrl").

-include("slim_mqtt.hrl").

-export([start/1]).

-export([start_link/1]).

-behaviour(gen_server).

-export([init/1,
    	handle_call/3,
    	handle_cast/2,
    	handle_info/2,
        code_change/3,
    	terminate/2]).

-define(SOCKET_OPTS, [
	binary,
	{packet,    raw},
	{reuseaddr, true},
	{backlog,   128},
	{nodelay,   false}]).

-record(state, {transport,
    			socket,
    			conn_name,
    			await_recv,
    			connection_state,
    			conserve,
    			parse_state,
                message_id,
                client_id,
                %clean_sess,
                %will_msg,
    			keep_alive, 
    			awaiting_ack,
                subtopics,
    			awaiting_rel,
				topic,
				ticket}).

-record(keep_alive, {state, period, timer, msg}).

start(Opts) ->
	Port = proplists:get_value(port, Opts, 8883),
	MqttPid = 
	case esockd:listen(mqtt, Port, ?SOCKET_OPTS, {?MODULE, start_link, []}) of
    {ok, Pid} -> Pid;
    {error, {already_started, Pid}} -> Pid
    end,
	?PRINT("mqtt listening on ~p~n", [Port]),
	{ok, MqttPid}.

start_link(Socket) ->
    proc_lib:start_link(?MODULE, init, [Socket]).
	
init(Sock) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = esockd_client:accepted(Sock),
    ok = gen_tcp:setopts(Sock, [{active, once}]),

	%%TODO: MOVE TO esockd acceptor...
    %%ok = throw_on_error(inet_error, fun () -> nextalk_net:tune_buffer_size(Sock) end),

    {ok, ConnStr} = slim_net:connection_string(Sock, inbound),
	?INFO("MQTT Conn: ~p", [ConnStr]),
    State =
      control_throttle(
       #state{ transport		= gen_tcp,
    		   socket           = Sock,
    		   conn_name        = ConnStr,
    		   await_recv       = false,
    		   connection_state = running,
    		   conserve         = false,
    		   parse_state      = slim_mqtt_frame:initial_state(),
    		   message_id		= 1,
    		   subtopics		= [],
    		   awaiting_ack		= gb_trees:empty(),
    		   awaiting_rel		= gb_trees:empty()}),
    gen_server:enter_loop(?MODULE, [], State).

handle_call(Req, _From, State) ->
    {stop, {badreq, Req}, State}.

handle_cast(Msg, State) ->
    {stop, {badmsg, Msg}, State}.

%Nextalk packet
handle_info({ok, Packets}, #state{transport = Transport, socket = Sock, topic=Topic} = State) ->
	?INFO("MQTT Got Packets: ~p", [Packets]),
	Payload = slim_packet:encode(Packets),
    Frame = #mqtt_frame{
    	fixed = #mqtt_frame_fixed{type 	 = ?PUBLISH,
    							  qos    = ?QOS_0,
    							  retain = 0,
    							  dup    = 0},
    	variable = #mqtt_frame_publish{topic_name = Topic},
    	payload = Payload},
    send_frame(Transport, Sock, Frame),
	{noreply, State};

handle_info(keep_alive_timeout, #state{keep_alive=KeepAlive}=State) ->
    case keepalive_state(KeepAlive) of
    idle ->
    	{stop, normal, State};
    active ->
    	KeepAlive1 = keepalive_reset(KeepAlive),
    	{noreply, State#state{keep_alive=KeepAlive1}}
    end;

handle_info({tcp, Sock, Data}, #state{socket = Sock} = State) ->
    %% when you're ready enable the next message  
    process_received_bytes(
      Data, control_throttle(State#state{ await_recv = false }));
    
handle_info({tcp_closed, Sock}, #state{socket = Sock} = State) ->
    network_error(tcp_closed, State);

handle_info(stop, State) ->
	{stop, normal, State};

handle_info(Info, State) ->
    {stop, {badinfo, Info}, State}.

terminate(_Reason, #state{keep_alive=KeepAlive}) ->
    keepalive_cancel(KeepAlive),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
throw_on_error(E, Thunk) ->
    case Thunk() of
    {error, Reason} -> throw({E, Reason});
    {ok, Res}       -> Res;
    Res             -> Res
    end.

%-------------------------------------------------------
% receive and parse tcp data
%-------------------------------------------------------
process_received_bytes(<<>>, State) ->
    {noreply, State};

process_received_bytes(Bytes,
                       State = #state{ parse_state = ParseState,
                                       conn_name   = ConnStr }) ->
    case slim_mqtt_frame:parse(Bytes, ParseState) of
    {more, ParseState1} ->
    	{noreply,
    	 control_throttle( State #state{ parse_state = ParseState1 }),
    	 hibernate};
    {ok, Frame, Rest} ->
    	case process_frame(Frame, State) of
    	{ok, State1} ->
    		PS = slim_mqtt_frame:initial_state(),
    		process_received_bytes(
    		  Rest,
    		  State1 #state{ parse_state = PS});
    	{err, Reason, State1} ->
    		?ERROR("MQTT protocol error ~p for connection ~p~n", [Reason, ConnStr]),
    		stop({shutdown, Reason}, State1);
    	{stop, State1} ->
    		stop(normal, State1)
    	end;
    {error, Error} ->
    	?ERROR("MQTT detected framing error ~p for connection ~p~n", [ConnStr, Error]),
    	stop({shutdown, Error}, State)
    end.

process_frame(Frame = #mqtt_frame{fixed = #mqtt_frame_fixed{type = Type}},
              State=#state{keep_alive=KeepAlive}) ->
    KeepAlive1 = keepalive_activate(KeepAlive),
    case validate_frame(Type, Frame) of	
    ok ->
    	%?INFO("frame from ~s: ~p", [ClientId, Frame]),
    	process_request(Type, Frame, State#state{keep_alive=KeepAlive1});
    {error, Reason} ->
    	{err, Reason, State}
    end.

process_request(?CONNECT,
                #mqtt_frame{ variable = #mqtt_frame_connect{
                                          username   = Domain,
                                          password   = STicket,
                                          proto_ver  = ProtoVersion,
    									  keep_alive = AlivePeriod,
                                          client_id  = ClientId }}, 
				#state{transport = Transport, socket = Sock} = State) ->
    {ReturnCode, State1} =
        case {ProtoVersion =:= ?MQTT_PROTO_MAJOR,
              valid_client_id(ClientId)} of
            {false, _} ->
                {?CONNACK_PROTO_VER, State};
            {_, false} ->
                {?CONNACK_INVALID_ID, State};
            _ ->
				Ticket = #slim_ticket{class=Class, id=Id} 
					= slim_ticket:make(list_to_binary(STicket)),
				Oid = slim_oid:make(Class, list_to_binary(Domain), Id),
				case slim_cm:lookup(Ticket) of
				Pid when is_pid(Pid) ->
					?INFO("connect from clientid: ~s, ~p", [ClientId, AlivePeriod]),
					%TODO: FIX
					slim_client:subscribe(Pid, Ticket),
					KeepAlive = keepalive_new(AlivePeriod*1500, keep_alive_timeout),
					{?CONNACK_ACCEPT,
					 State #state{client_id  = ClientId,
								  topic = slim_oid:topic(Oid),
								  ticket = Ticket,
								  keep_alive = KeepAlive}};
				undefined ->
					?ERROR("MQTT connect failed: domain=~s, ticket=~s", [Domain, STicket]),
					{?CONNACK_CREDENTIALS, State}
				end
        end,
    	send_frame(Transport, Sock, #mqtt_frame{ fixed    = #mqtt_frame_fixed{ type = ?CONNACK},
    							 variable = #mqtt_frame_connack{
                                         return_code = ReturnCode }}),
    {ok, State1};

process_request(?PUBLISH, Frame, State) ->
	?INFO("ignore PUBLISH: ~n~p", [Frame]),
    {ok, State};

process_request(?PUBACK, Frame, State) ->
	?INFO("ignore PUBLACK: ~n~p", [Frame]),
    {ok, State};

process_request(?PUBREC, Frame, State) ->
	?INFO("ignore PUBLREC: ~n~p", [Frame]),
    {ok, State};

process_request(?PUBREL, Frame, State) ->
	?INFO("ignore PUBLREL: ~n~p", [Frame]),
    {ok, State};

process_request(?PUBCOMP, Frame, State) ->
	?INFO("ignore PUBCOMP: ~n~p", [Frame]),
    {ok, State};

process_request(?SUBSCRIBE, Frame, State=#state{conn_name=ConnName}) ->
	?INFO("ignore SUBSCRIBE from ~s: ~n~p", [ConnName, Frame]),
    {ok, State};

process_request(?UNSUBSCRIBE, Frame, State=#state{conn_name=ConnName}) ->
	?INFO("ignore UNSUBSCRIBE from ~s: ~n~p", [ConnName, Frame]),
    {ok, State};

process_request(?PINGREQ, #mqtt_frame{}, #state{transport = Transport, socket=Sock, keep_alive=KeepAlive}=State) ->
    %Keep alive timer
    KeepAlive1 = keepalive_reset(KeepAlive),
    send_frame(Transport, Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{ type = ?PINGRESP }}),
    {ok, State#state{keep_alive=KeepAlive1}};

process_request(?DISCONNECT, #mqtt_frame{}, State) ->
    {stop, State}.

%%----------------------------------------------------------------------------

send_frame(Transport, Sock, Frame) ->
    Transport:send(Sock, slim_mqtt_frame:serialise(Frame)).
    %erlang:port_command(Sock, slim_mqtt_frame:serialise(Frame)).

%%----------------------------------------------------------------------------
network_error(Reason,
              State = #state{ conn_name  = ConnStr}) ->
    ?INFO("MQTT detected network error '~p' for ~p", [Reason, ConnStr]),
    % todo: flush channel after publish
    stop({shutdown, conn_closed}, State).

run_socket(State = #state{ connection_state = blocked }) ->
    State;
run_socket(State = #state{ await_recv = true }) ->
    State;
run_socket(State = #state{ transport = Transport, socket = Sock }) ->
    %async_recv(Sock, 0, infinity),
    %inet:setopts(Sock, [{active, once}]),
    Transport:setopts(Sock, [{active, once}]),
    State#state{ await_recv = true }.

control_throttle(State = #state{ connection_state = Flow,
                                 conserve         = Conserve }) ->
    case {Flow, Conserve} of
        {running,   true} -> State #state{ connection_state = blocked };
        {blocked,  false} -> run_socket(State #state{
                                                connection_state = running });
        {_,            _} -> run_socket(State)
    end.

stop(Reason, State ) ->
    {stop, Reason, State}.

valid_client_id(ClientId) ->
    ClientIdLen = length(ClientId),
    1 =< ClientIdLen andalso ClientIdLen =< ?CLIENT_ID_MAXLEN.


validate_frame(?PUBLISH, #mqtt_frame{variable = #mqtt_frame_publish{topic_name = Topic}}) ->
    case slim_topic:validate({publish, list_to_binary(Topic)}) of
    true -> ok;
    false -> {error, badtopic}
    end;

validate_frame(?UNSUBSCRIBE, #mqtt_frame{variable = #mqtt_frame_subscribe{topic_table = Topics}}) ->
    ErrTopics = [Topic || #mqtt_topic{name=Topic} <- Topics,
    					not slim_topic:validate({subscribe, list_to_binary(Topic)})],
    case ErrTopics of
    [] -> ok;
    _ -> ?ERROR("error topics: ~p", [ErrTopics]), {error, badtopic}
    end;

validate_frame(?SUBSCRIBE, #mqtt_frame{variable = #mqtt_frame_subscribe{topic_table = Topics}}) ->
    ErrTopics = [Topic || #mqtt_topic{name=Topic, qos=Qos} <- Topics,
    					not (slim_topic:validate({subscribe, list_to_binary(Topic)}) and (Qos < 3))],
    case ErrTopics of
    [] -> ok;
    _ -> ?ERROR("error topics: ~p", [ErrTopics]), {error, badtopic}
    end;

validate_frame(_Type, _Frame) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%keep alive handler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
keepalive_new(undefined, _) -> 
	undefined;
keepalive_new(0, _) -> 
	undefined;
keepalive_new(Period, TimeoutMsg) when is_integer(Period) ->
	Ref = erlang:send_after(Period, self(), TimeoutMsg),
	#keep_alive{state=idle, period=Period, timer=Ref, msg=TimeoutMsg}.

keepalive_state(undefined) -> 
	undefined;
keepalive_state(#keep_alive{state=State}) ->
	State.

keepalive_activate(undefined) -> 
	undefined; 
keepalive_activate(KeepAlive) when is_record(KeepAlive, keep_alive) -> 
	KeepAlive#keep_alive{state=active}.

keepalive_reset(undefined) ->
	undefined;
keepalive_reset(KeepAlive=#keep_alive{period=Period, timer=Timer, msg=Msg}) ->
	catch erlang:cancel_timer(Timer),
	Ref = erlang:send_after(Period, self(), Msg),
	KeepAlive#keep_alive{state=idle, timer = Ref}.

keepalive_cancel(undefined) -> 
	undefined;
keepalive_cancel(KeepAlive=#keep_alive{timer=Timer}) -> 
	catch erlang:cancel_timer(Timer),
	KeepAlive#keep_alive{timer=undefined}.

