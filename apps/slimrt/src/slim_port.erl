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

-module(slim_port).

-author('feng.lee@slimchat.io').

-export([open/3,
		 addr/1]).

%SockOpts = [binary,
%			{packet,    raw},
%			{reuseaddr, true},
%			{backlog,   128},
%			{nodelay,   true}],
open(Name, Module, Opts) ->
    Listener = case ranch:start_listener(Name, 10, ranch_tcp, Opts, Module, []) of
    {ok, Pid} -> Pid;
    {error, {already_started, Pid}} -> Pid
    end,
	%?PRINT("Mqttd is listening on ~p~n", [proplists:get_value(port, Opts)]),
	{ok, Listener}.


%TODO:   
addr(jsonp) ->
	<<"http://localhost:8080/v1/packets">>;

addr(mqtt) ->
	<<"tcp://localhost:8883">>;

addr(wsocket) ->
	<<"http://localhost:8080/v1/wsocket">>.  

