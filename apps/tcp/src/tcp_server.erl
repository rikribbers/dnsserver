%%%-------------------------------------------------------------------
%%% @author rik.ribbers
%%% @copyright
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc
%%%
%%% TCP socket server worker more or less based on:
%%% http://learnyousomeerlang.com/buckets-of-sockets#sockserv-revisited
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_server).
-author("rik.ribbers").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% socket         : The socket
%% close_socket  : Close the socket after each request
%% timeout       : Timeout on request
%% function      : Function to call when data is received
-record(state, {socket, close_socket, timeout, function}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ListenSocket :: socket) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ListenSocket) ->
  gen_server:start_link(?MODULE, [ListenSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ListenSocket]) ->
  lager:debug("init ListenSocket=~p", [ListenSocket]),
  {ok, CloseSocket} = application:get_env(tcp,tcp_close_socket),
  {ok, {Module, FunctionName, Arity}} = application:get_env(tcp,tcp_function),
  Function = fun Module:FunctionName/Arity,
  %% LATER : Implement timeout
  Timeout = 1000,

  %% Because accepting a connection is a blocking function call,
  %% we can not do it in here. Forward to the server loop!
  gen_server:cast(self(), accept),

  %% First pass the listen socket.
  {ok, #state{socket = ListenSocket, close_socket = CloseSocket, function = Function, timeout = Timeout}, 0}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  %% Not needed let caller timeout
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(accept, State = #state{socket = ListenSocket}) ->
  lager:debug("cast=accept State=~p", [State]),
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  lager:debug("cast=accept AcceptSocket=~p", [AcceptSocket]),
  %% Remember that thou art dust, and to dust thou shalt return.
  %% We want to always keep a given number of children in this app.
  tcp_accept_server_sup:start_accept_socket(), % a new acceptor is born, praise the lord
  %% Set active once
  inet:setopts(AcceptSocket, [{active, once}]),

  %% Now that we have a active connection on the ActiveSocket replace
  %% the ListenSocket with the AcceptSocket in this gen_servers State
  %% The ListenSocket becomes available for accepting new connections
  %% and no longer needed here.
  {noreply, State#state{socket = AcceptSocket}};

handle_cast(stop, State) ->
  lager:debug("stop"),
  {stop, normal, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({tcp, Socket, RawData}, State) ->
  lager:info("info=tcp RawData=~p Socket=~p", [RawData, Socket]),
  %% The difference between an active and a passive socket is that
  %% an active socket will send incoming data as Erlang messages, while
  %% passive sockets will require to be polled with gen_tcp:recv/2-3.
  %%
  %% Depending on the context, you might want one or the other. Active
  %% sockets are easier to work
  %% with. However, one problem with active sockets is that all input
  %% is blindly changed into messages and makes it so the Erlang VM
  %% is somewhat more subject to overload. Passive sockets push this
  %% responsibility to the underlying implementation and the OS and are
  %% somewhat safer.
  %%
  %% A middle ground exists, with sockets that are 'active once'.
  %% The {active, once} option (can be set with inet:setopts or
  %% when creating the listen socket) makes it so only *one* message
  %% will be sent in active mode, and then the socket is automatically
  %% turned back to passive mode. On each message reception, we turn
  %% the socket back to {active once} as to achieve rate limiting.

  inet:setopts(Socket, [{active, once}]),

  {ok, NewState} = handle_data(Socket, RawData, State),
  case NewState#state.close_socket of
    true ->
      {stop, normal, NewState};
    false ->
      {noreply, NewState}
  end;

handle_info({tcp_closed, Socket}, State) ->
  lager:debug("info=tcp_closed Socket=~p", [Socket]),
  {stop, normal, State};

handle_info({tcp_error, Socket}, State) ->
  lager:debug("info=tcp_error Socket=~p", [Socket]),
  {stop, normal, State};

handle_info(E, S) ->
  lager:error("info=unexpected: ~p~n", [E]),
  {noreply, S}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, State) ->
  Socket = State#state.socket,
  lager:debug("terminate Socket=~p", [Socket]),
  gen_tcp:close(Socket),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Internal functions
-spec(handle_data(Socket :: socket, RawData :: term(), State :: #state{})
      -> {ok, NewState :: #state{}}).
handle_data(Socket, RawData, State) ->
  Function = State#state.function,
  lager:debug("handle_data Function=~p", [Function]),
  {ok, CloseSocket} = Function(Socket, RawData),
  {ok, State#state{close_socket = CloseSocket}}.
