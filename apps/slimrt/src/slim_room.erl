-module(slim_room).

-author('feng.lee@slimchat.io').

-include("slimrt.hrl").

-export([start_link/0]).

-export([members/1, 
		rooms/1,
        join/4,
		leave/2,
        leave/3]).

-behavior(gen_server).

-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3]).

%Gid: Room Oid

%%
%% @doc start grpchat.
%%
-spec start_link() -> {ok, pid()}.
start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% @doc Get members of a room
%% 
-spec members(Gid :: oid()) -> list(#slim_room{}).
members(Gid) when is_record(Gid, slim_oid) ->
    mnesia:dirty_read(slim_room, Gid).

%%
%% @doc Get rooms of an endpoint
%%
-spec rooms(Oid :: oid()) -> list(#slim_room{}).
rooms(Oid) when is_record(Oid, slim_oid) ->
    mnesia:dirty_index_read(slim_room, Oid, #slim_room.oid).

%%
%% @doc An endpoint join a room.
%%
-spec join(Gid :: oid(), Oid :: oid(), Pid :: pid(), Nick :: binary()) -> any().
join(Gid, Oid, Pid, Nick) when is_record(Gid, slim_oid) ->
	join([Gid], Oid, Pid, Nick);

join(Gids, Oid, Pid, Nick) when is_list(Gids) ->
    %old gids 
    %OldGids = [  Id || #slim_room{gid = Id} <- mnesia:dirty_index_read(slim_room, Oid, #slim_room.oid) ],

	%write into db
	Room = fun(Gid) -> #slim_room{gid=Gid, oid=Oid, nick=Nick} end,
	mnesia:sync_dirty(fun() -> [mnesia:write(Room(Gid)) || Gid <- Gids] end),

    %broadcast 'join' presence to new joined rooms
    %JoinedGids = Gids -- OldGids,
    %Presence = fun(Gid) -> 
    %    #slim_presence{
    %        type = join,
    %        to = Gid,
    %        from = Oid,
    %        nick = Nick,
    %        show = <<"available">>,
    %        status = slim_oid:name(Gid)
    %    }
    %end,
    %[slim_router:route(Oid, Gid, Presence(Gid)) || Gid <- JoinedGids], 

	%subscribe to router
	[ slim_pubsub:subscribe(slim_oid:topic(Gid), Pid) || Gid <- Gids ].

%%
%% @doc An endpoint leave a room.
%%
-spec leave(Gid :: oid(), Oid :: oid(), Pid :: pid()) -> any().
leave(Gid, Oid, Pid) ->
	%unsubscribe
	slim_pubsub:unsubscribe(slim_oid:topic(Gid), Pid),
    %read rooms
    Rooms = [ G || G = #slim_room{oid = Oid1}
			<- mnesia:dirty_read({slim_room, Gid}), Oid == Oid1],
	%remove from db
	mnesia:sync_dirty(fun() ->
		[ mnesia:delete_object(G) || G <- Rooms ]
	end).
    %broadcast 'leave' presences, TODO: need to test
    %Presence = fun(#slim_room{gid = Gid1, nick = Nick}) -> 
	%    #slim_presence{
    %        type = leave,
    %        to   = Gid1,
    %        from = Oid,
    %        nick = Nick,
    %        show = <<"available">>,
    %        status = slim_oid:name(Gid1)}
    %end,
    %[slim_router:route(Oid, Gid, Presence(Room)) || Room <- Rooms].

%%
%% @doc An endpoint leave all rooms
%%
-spec leave(Oid :: oid(), Pid :: pid()) -> any().
leave(Oid, Pid) ->
	%unsubscribe
	Rooms = mnesia:dirty_index_read(slim_room, Oid, #slim_room.oid),
	[ slim_pubsub:unsubscribe(slim_oid:topic(Gid), Pid) || #slim_room{ gid = Gid } <- Rooms ],
	%remove from db
	mnesia:sync_dirty(fun() -> [ mnesia:delete_object(G) || G <- Rooms ] end).
    %broadcast 'leave' presence, TODO: need to test
    %Presence = fun(#slim_room{gid = Gid, nick = Nick}) -> 
	%    #slim_presence{
    %        type = leave,
    %        to   = Gid,
    %        from = Oid,
    %        nick = Nick,
    %        show = <<"available">>,
    %        status = slim_oid:name(Gid)}
    %end,
    %[slim_router:route(Oid, Gid, Presence(Room)) || Room = #slim_room{gid=Gid} <- Rooms].


init([]) ->
    mnesia:create_table(slim_room,
        [{ram_copies, [node()]},
         {type, bag},
         {index, [oid]},
         {attributes, record_info(fields, slim_room)}]),
    mnesia:add_table_copy(slim_room, node(), ram_copies),
    {ok, state}.

handle_call(Req, _From, State) ->
    {stop, {badreq, Req}, State}.

handle_cast(Msg, State) ->
    {stop, {badmsg, Msg}, State}.

handle_info(Info,State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

