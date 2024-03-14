% NOTE: see 3.8 in documentaion for important notes
% I pledge my honor that I have abided by the Stevens Honors System
% Connor Ivancik
% Gianna Mancuso

-module(client).

-export([main/1, initial_state/2]).

-include_lib("./defs.hrl").

-spec main(_InitialState) -> _.
-spec listen(_State) -> _.
-spec initial_state(_Nick, _GuiName) -> _InitialClientState.
-spec loop(_State, _Request, _Ref) -> _.
-spec do_join(_State, _Ref, _ChatName) -> _.
-spec do_leave(_State, _Ref, _ChatName) -> _.
-spec do_new_nick(_State, _Ref, _NewNick) -> _.
-spec do_new_incoming_msg(_State, _Ref, _SenderNick, _ChatName, _Message) -> _.

%% Receive messages from GUI and handle them accordingly
%% All handling can be done in loop(...)
main(InitialState) ->
    %% The client tells the server it is connecting with its initial nickname.
    %% This nickname is guaranteed unique system-wide as long as you do not assign a client
    %% the nickname in the form "user[number]" manually such that a new client happens
    %% to generate the same random number as you assigned to your client.
    whereis(server)!{self(), connect, InitialState#cl_st.nick},
    %% if running test suite, tell test suite that client is up
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{client_up, self()}
    end,
    %% Begins listening
    listen(InitialState).

%% This method handles all incoming messages from either the GUI or the
%% chatrooms that are not directly tied to an ongoing request cycle.
listen(State) ->
    receive
        {request, From, Ref, Request} ->
	    %% the loop method will return a response as well as an updated
	    %% state to pass along to the next cycle
            {Response, NextState} = loop(State, Request, Ref),
	    case Response of
		{dummy_target, Resp} ->
		    io:format("Use this for whatever you would like~n"),
		    From!{result, self(), Ref, {dummy_target, Resp}},
		    listen(NextState);
		%% if shutdown is received, terminate
		shutdown ->
		    ok_shutdown;
		%% if ok_msg_received, then we don't need to reply to sender.
		ok_msg_received ->
		    listen(NextState);
		%% otherwise, reply to sender with response
		_ ->
		    From!{result, self(), Ref, Response},
		    listen(NextState)
	    end
    end.

%% This function just initializes the default state of a client.
%% This should only be used by the GUI. Do not change it, as the
%% GUI code we provide depends on it.
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, con_ch = maps:new() }.

%% ------------------------------------------
%% loop handles each kind of request from GUI
%% ------------------------------------------
loop(State, Request, Ref) ->
    case Request of
	%% GUI requests to join a chatroom with name ChatName
	{join, ChatName} ->
	    do_join(State, Ref, ChatName);

	%% GUI requests to leave a chatroom with name ChatName
	{leave, ChatName} ->
	    do_leave(State, Ref, ChatName);

	%% GUI requests to send an outgoing message Message to chatroom ChatName
	{outgoing_msg, ChatName, Message} ->
	    do_msg_send(State, Ref, ChatName, Message);

	%% GUI requests the nickname of client
	whoami ->
	    {{dummy_target, dummy_response}, State};
		%TODO
		% 1. send {result, self(), Ref, Nickname} to GUI where nickname is in cl_st

	%% GUI requests to update nickname to Nick
	{nick, Nick} ->
            do_new_nick(State, Ref, Nick);

	%% GUI requesting to quit completely
	quit ->
	    do_quit(State, Ref);

	%% Chatroom with name ChatName has sent an incoming message Message
	%% from sender with nickname SenderNick
	{incoming_msg, SenderNick, ChatName, Message} ->
	    do_new_incoming_msg(State, Ref, SenderNick, ChatName, Message);

	{get_state} ->
	    {{get_state, State}, State};

	%% Somehow reached a state where we have an unhandled request.
	%% Without bugs, this should never be reached.
	_ ->
	    io:format("Client: Unhandled Request: ~w~n", [Request]),
	    {unhandled_request, State}
    end.

%% executes `/join` protocol from client perspective
do_join(State, Ref, ChatName) ->
    io:format("client:do_join(...): IMPLEMENT ME~n"),
    {{dummy_target, dummy_response}, State}.
%%TODO
% 1. GUI sends message {request, self(), Ref, {join, ChatName}} to client
% 2. Check to see if already in chatroom
% 2a. if in already in room, message {result, self(), Ref, err} to GUI, end
% 2b. if NOT in room already, send {self(), Ref, join, ChatName} to server
% 3. recieve message from chatroom, then update recird if connected chatrooms
% 4. send {result, self(), Ref, History} to GUI

%% executes `/leave` protocol from client perspective
do_leave(State, Ref, ChatName) ->
    io:format("client:do_leave(...): IMPLEMENT ME~n"),
    {{dummy_target, dummy_response}, State}.
	%TODO
	% 1. recieve {request, self(), Ref, {leave, Chatname} from GUI
	% 2. check to see if you are in the chatroom using ChatName
	% 3. if chatroom is not found, send {result, self(), Ref, err} to GUI and end
	% 4. if chatroom is found, client should send {self(), Ref, leave, ChatName} to server
	% 5. recieve {self, Ref, ack_leave} from chatroom
	% 6. remove chatroom from list of chatrooms
	% 7. send {result, self(), Ref, ok} to GUI

%% executes `/nick` protocol from client perspective
do_new_nick(State, Ref, NewNick) ->
    io:format("client:do_new_nick(...): IMPLEMENT ME~n"),
    {{dummy_target, dummy_response}, State}.
	%TODO
	% 1. check NewNick against current nickname
	% 2. if newnick is the same as the current nickname, send {result, self(), Ref, err_same} to GUI and end
	% 3. otherwise, send {self(), Ref, nick, NewNick} to server
	% 4. if recieve {self(), Ref, err_nick_used} from server, send {result, self(), Ref, err_nick_used} to GUI
	% 5. recieve {self(), Ref, ok_nick} from server
	% 6. send {result, self(), Ref, ok_nick} to GUI
%% executes send message protocol from client perspective
do_msg_send(State, Ref, ChatName, Message) ->
    io:format("client:do_new_nick(...): IMPLEMENT ME~n"),
    {{dummy_target, dummy_response}, State}.
	%TODO
	% 1. look up PID of the chatroom
	% 2. send the message {self(), Ref, message, Message} to chatroom
	% 3. recieve {self(), Ref, ack_msg} from chatroom
	% 4. send {result, self(), Ref, {msg_sent, St#cl_st.nick}} to GUI
%% executes new incoming message protocol from client perspective
do_new_incoming_msg(State, _Ref, CliNick, ChatName, Msg) ->
    %% pass message along to gui
    gen_server:call(list_to_atom(State#cl_st.gui), {msg_to_GUI, ChatName, CliNick, Msg}),
    {ok_msg_received, State}.

%% executes quit protocol from client perspective
do_quit(State, Ref) ->
    io:format("client:do_new_nick(...): IMPLEMENT ME~n"),
    {{dummy_target, dummy_response}, State}.
	%TODO
	% NOTE: GUI is buggy and will sometimes have nothing happen when you enter the command, mainly GUI window wont close
	% 1. send {self(), Ref, quit} to server
	% NOTE: will recieve unregister messages from chatroom, these should be ignored
	% 2. recieve {self, Ref, ack_quit} from server
	% 3. send {self, Ref, ack_quit} to GUI
	% 4. "cleanly exit" <-- whatever that means