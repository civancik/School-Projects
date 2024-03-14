-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
    io:format("server:do_join(...): IMPLEMENT ME~n"),
    State.
	%TODO
	% 1. Recieve {self(), Ref, join, ChatName} from client
	% 2. check if chatroom exists yet using chatrooms within serv_st, if it does not exist, spawn room
	% 3. look up clients nickname from the server serv_st record
	% 4. send message to chatroom that is {self(), Ref, register, ClientPID, ClientNick}
% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    io:format("server:do_leave(...): IMPLEMENT ME~n"),
    State.
	%TODO
	% 1. recieve message from client
	% 2. look up chatrooms PID from the server's state serv_st
	% 3. remove client from local record of chatroom registrations
	% 4. send message {self(), Ref, unregister, CLientPID} to chatroom

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    io:format("server:do_new_nick(...): IMPLEMENT ME~n"),
    State.
    %TODO
	% 1. check if NewNick is already in use, if so send {self(), Ref, err_nick_used} to client and end
	% 2. update record of nicknames by pointing the clientsPID to the new nickname
	% 3. update all chatrooms of new nickname. to do so, send {self(), Ref, update_nick, ClientPID, NewNick} to the EACH relevant chatroom
	%
%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    io:format("server:do_client_quit(...): IMPLEMENT ME~n"),
    State.
	%TODO
	% NOTE: cleanup time
	% 1. remove client from nicknames
	% 2. send EACH chatroom that has client {self(), Ref, unregister, ClientPID}
	% NOTE: step 2 is the same as leaving a chatroom normally, thus this should be handled on the server side when leave is implemented
	% 3. remove client from the chat registration
	% 4. send {self(), Ref, ack_quit} to client