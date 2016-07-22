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
%%% TCP socket server supervisor more or less based on:
%%% http://learnyousomeerlang.com/buckets-of-sockets#sockserv-revisited
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_accept_server_sup).
-author("rik.ribbers").

-behaviour(supervisor).

%% API
-export([start_link/0, start_accept_socket/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: supervisor:sup_flags(),
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore ).
init([]) ->
  {ok, Port} = application:get_env(tcp,tcp_port),
  {ok, NofAcceptSockets} = application:get_env(tcp,tcp_nof_accept_sockets),
  lager:info("Init tcp_accept_server_sup on Port=~p NofAcceptSockets=~p...", [Port, NofAcceptSockets]),

  %% Create the listen socket
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, inet6]),
  lager:debug("ListenSocket=~p", [ListenSocket]),

  Child = {tcp_server,
    {tcp_server, start_link, [ListenSocket]}, % pass the socket!
    temporary, 30000, worker, [tcp_server]},

  spawn_link(fun() -> [start_accept_socket() || _ <- lists:seq(1, NofAcceptSockets)] end),

  {ok, {{simple_one_for_one, 60, 3600},
    [Child]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Start a child process that accept socket connections
start_accept_socket() ->
  supervisor:start_child(?MODULE, []).
