
-define(APIVSN, "v1").  
%TODO: THIS IS STUPID


%
% online
%
-define(API_ONLINE, "/presences/online").

%
% show
%
-define(API_SHOW, "/presences/show").

%
% offline
%
-define(API_OFFLINE, "/presences/offline").

%
% presences
%
-define(API_PRESENCES, "/presences").

-define(API_OPEN, "/chats/open").

-define(API_CLOSE, "/chats/close").

%
% status
%
-define(API_STATUSES, "/statuses").

%
% message
%
-define(API_MESSAGES, "/messages").

%
% join group
%
-define(API_ROOM_JOIN, "/rooms/:id/join").

%
% leave group
%
-define(API_ROOM_LEAVE, "/rooms/:id/leave").

%
% group members
%
-define(API_ROOM_MEMBERS, "/rooms/:id/members").

-define(HTTP_APIS, [
	?API_ONLINE,
	?API_OFFLINE,
	?API_SHOW,
	?API_PRESENCES,
	?API_OPEN,
	?API_CLOSE,
	?API_STATUSES,
	?API_MESSAGES,
	?API_ROOM_JOIN,
	?API_ROOM_LEAVE,
	?API_ROOM_MEMBERS]).

%Jsonp long poll
-define(API_JSONP, "/packets").

%Websocket long connect
-define(API_WSOCKET, "/wsocket").



