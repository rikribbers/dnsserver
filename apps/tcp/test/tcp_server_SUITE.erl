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
%%% TCP socket server test
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_server_SUITE).
-author("rik.ribbers").

-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([normal_behaviour_test/1, child_test/1,
  running_echo_server_test/1, close_socket_test/1]).


all() ->
  [normal_behaviour_test, child_test,
    running_echo_server_test,close_socket_test].

init_per_testcase(running_echp_server,Config) ->
  tcp_test_utils:set_environment_variables(),
  %% make sure the echo server is running before
  %% tcp_sup is started
  tcp_echo_server:start_link(),
  %% now do normal initialisation
  init_per_testcase([],Config);

%% Override default setting and set tcp_close_socket=true
init_per_testcase(close_socket_test, Config) ->
  tcp_test_utils:set_environment_variables(),
  application:set_env(tcp,tcp_close_socket,true),

  ct:log(sys_state, "Starting the supervisor~n"),
  {ok, SuperPid} = tcp_sup:start_link(),
  [{supervisor, SuperPid}| Config];

init_per_testcase(_TestCase, Config) ->
  tcp_test_utils:set_environment_variables(),

  ct:log(sys_state, "Starting the supervisor~n"),
  {ok, SuperPid} = tcp_sup:start_link(),
  [{supervisor, SuperPid}| Config].

end_per_testcase(_TestCase, Config) ->
  ct:log(sys_state, "Stopping the supervisor~n"),

  %% interesting read: http://erlang.org/pipermail/erlang-questions/2010-June/051869.html
  %% so unlink the process as the supervisor is linked
  %% to the CT process
  SuperPid = ?config(supervisor, Config),
  unlink(SuperPid),

  %% We need to extract the listen socket and close it explicitly here
  %% as this is *NOT* closed when the application is shutdown (but when erts quits)
  {links,A1} = erlang:process_info(erlang:whereis(tcp_accept_server_sup),links),
  [Port|_Tail] = [X || X <- A1, is_port(X)],
  %% first exit supervisor
  exit(SuperPid, normal),

  %% next close listen socket
  gen_tcp:close(Port).

%% Test the number of accepting sockets
child_test(_Config) ->
  {ok, Port} = application:get_env(tcp,tcp_port),
  {ok, N} = application:get_env(tcp,tcp_nof_accept_sockets),
  {links,A1} = erlang:process_info(erlang:whereis(tcp_accept_server_sup),links),
  %% 2 extra processes, one for the ListenSocket and one for its own supervisor
  2 = length(A1) - N,

  %% open a socket, that creates a new process
  {ok, Socket1} = gen_tcp:connect("::1", Port, [inet6,{active,true}]),
  ct:sleep(1),
  {links,A2} = erlang:process_info(erlang:whereis(tcp_accept_server_sup),links),
  3 = length(A2) - N,

  %% open another socket
  {ok, Socket2} = gen_tcp:connect("::1", Port, [inet6,{active,true}]),
  ct:sleep(1),
  {links,A3} = erlang:process_info(erlang:whereis(tcp_accept_server_sup),links),
  4  = length(A3) - N,

  %% close a socket
  ok = gen_tcp:close(Socket1),
  ct:sleep(1),
  {links,A4} = erlang:process_info(erlang:whereis(tcp_accept_server_sup),links),
  3 = length(A4) - N,

  %% close another socket
  ok = gen_tcp:close(Socket2),
  ct:sleep(1),
  {links,A5} = erlang:process_info(erlang:whereis(tcp_accept_server_sup),links),
  2 = length(A5) - N.

%% test that the configured tcp server is already configured.
running_echo_server_test(_Config) ->
  generic_send_data("::1",inet6,{0,0,0,0,0,0,0,1}),
  generic_send_data("127.0.0.1",inet,{127,0,0,1}).

%% test normal behaviour
normal_behaviour_test(_Config) ->
  generic_send_data("::1",inet6,{0,0,0,0,0,0,0,1}),
  generic_send_data("127.0.0.1",inet,{127,0,0,1}).

%% Test that socket remains open
close_socket_test(_Config) ->
  {ok, Port} = application:get_env(tcp,tcp_port),

  {ok, Socket2} = gen_tcp:connect("::1", Port, [inet6,{active,true}]),
  ok = gen_tcp:send(Socket2, "Some Data"),
  receive
    Any2  ->
      {tcp,_Socket2,"Some Data"}  = Any2
  after 1000 ->
    gen_tcp:close(Socket2),
    ct:fail("timeout")
  end,
  receive
    Any3  ->
      {tcp_closed,Socket2}  = Any3
  after 1000 ->
    gen_tcp:close(Socket2),
    ct:fail("timeout")
  end,
  ok = gen_tcp:close(Socket2).

generic_send_data(IpAsList,InetType,IpAsTuple) ->
  {ok, Port} = application:get_env(tcp,tcp_port),

  {ok, Socket2} = gen_tcp:connect(IpAsList, Port, [InetType,{active,true}]),
  {ok,{IpAsTuple,_}} = inet:sockname(Socket2),
  ok = gen_tcp:send(Socket2, "Some Data"),
  receive
    Any2  ->
      {tcp,_Socket2,"Some Data"}  = Any2
  after 1000 ->
    gen_tcp:close(Socket2),
    ct:fail("timeout")
  end,
  %% we don't expect a tcp_closed here.
  receive
    Any3  ->
      {tcp_closed,Socket2}  = Any3,
      ct:fail("socket closed unexpected")
  after 100 ->
    gen_tcp:close(Socket2)
  end,

  gen_tcp:close(Socket2).

