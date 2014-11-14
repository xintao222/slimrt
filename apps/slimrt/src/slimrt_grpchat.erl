-module(slimrt_grpchat).

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
-spec members(Gid :: oid()) -> list(#nextalk_room{}).
members(Gid) when is_record(Gid, nextalk_oid) ->
    mnesia:dirty_read(nextalk_room, Gid).

%%
%% @doc Get rooms of an endpoint
%%
-spec rooms(Oid :: oid()) -> list(#nextalk_room{}).
rooms(Oid) when is_record(Oid, nextalk_oid) ->
    mnesia:dirty_index_read(nextalk_room, Oid, #nextalk_room.oid).

%%
%% @doc An endpoint join a room.
%%
-spec join(Gid :: oid(), Oid :: oid(), Pid :: pid(), Nick :: binary()) -> any().
join(Gid, Oid, Pid, Nick) when is_record(Gid, nextalk_oid) ->
	join([Gid], Oid, Pid, Nick);

join(Gids, Oid, Pid, Nick) when is_list(Gids) ->
    %old gids 
    %OldGids = [  Id || #nextalk_room{gid = Id} <- mnesia:dirty_index_read(nextalk_room, Oid, #nextalk_room.oid) ],

	%write into db
	Room = fun(Gid) -> #nextalk_room{gid=Gid, oid=Oid, nick=Nick} end,
	mnesia:sync_dirty(fun() -> [mnesia:write(Room(Gid)) || Gid <- Gids] end),

    %broadcast 'join' presence to new joined rooms
    %JoinedGids = Gids -- OldGids,
    %Presence = fun(Gid) -> 
    %    #nextalk_presence{
    %        type = join,
    %        to = Gid,
    %        from = Oid,
    %        nick = Nick,
    %        show = <<"available">>,
    %        status = nextalk_oid:name(Gid)
    %    }
    %end,
    %[nextalk_router:route(Oid, Gid, Presence(Gid)) || Gid <- JoinedGids], 

	%subscribe to router
	[ nextalk_pubsub:subscribe(nextalk_oid:topic(Gid), Pid) || Gid <- Gids ].

%%
%% @doc An endpoint leave a room.
%%
-spec leave(Gid :: oid(), Oid :: oid(), Pid :: pid()) -> any().
leave(Gid, Oid, Pid) ->
	%unsubscribe
	nextalk_pubsub:unsubscribe(nextalk_oid:topic(Gid), Pid),
    %read rooms
    Rooms = [ G || G = #nextalk_room{oid = Oid1}
			<- mnesia:dirty_read({nextalk_room, Gid}), Oid == Oid1],
	%remove from db
	mnesia:sync_dirty(fun() ->
		[ mnesia:delete_object(G) || G <- Rooms ]
	end).
    %broadcast 'leave' presences, TODO: need to test
    %Presence = fun(#nextalk_room{gid = Gid1, nick = Nick}) -> 
	%    #nextalk_presence{
    %        type = leave,
    %        to   = Gid1,
    %        from = Oid,
    %        nick = Nick,
    %        show = <<"available">>,
    %        status = nextalk_oid:name(Gid1)}
    %end,
    %[nextalk_router:route(Oid, Gid, Presence(Room)) || Room <- Rooms].

%%
%% @doc An endpoint leave all rooms
%%
-spec leave(Oid :: oid(), Pid :: pid()) -> any().
leave(Oid, Pid) ->
	%unsubscribe
	Rooms = mnesia:dirty_index_read(nextalk_room, Oid, #nextalk_room.oid),
	[ nextalk_pubsub:unsubscribe(nextalk_oid:topic(Gid), Pid) || #nextalk_room{ gid = Gid } <- Rooms ],
	%remove from db
	mnesia:sync_dirty(fun() -> [ mnesia:delete_object(G) || G <- Rooms ] end).
    %broadcast 'leave' presence, TODO: need to test
    %Presence = fun(#nextalk_room{gid = Gid, nick = Nick}) -> 
	%    #nextalk_presence{
    %        type = leave,
    %        to   = Gid,
    %        from = Oid,
    %        nick = Nick,
    %        show = <<"available">>,
    %        status = nextalk_oid:name(Gid)}
    %end,
    %[nextalk_router:route(Oid, Gid, Presence(Room)) || Room = #nextalk_room{gid=Gid} <- Rooms].


init([]) ->
    mnesia:create_table(nextalk_room,
        [{ram_copies, [node()]},
         {type, bag},
         {index, [oid]},
         {attributes, record_info(fields, nextalk_room)}]),
    mnesia:add_table_copy(nextalk_room, node(), ram_copies),
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

