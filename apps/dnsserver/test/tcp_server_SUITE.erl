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
%%% TODO make child_test work
%%% TODO TEST with close socket configuration
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_server_SUITE).
-author("rik.ribbers").

-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([tcp_test/1,child_test/1]).


all() ->
  [tcp_test].

init_per_testcase(_TestCase, Config) ->
  test_utils:set_environment_variables(),

  %% do not the used port as the ListenSocket is not closed in CT env
  %% This ensure that it can be used else where if needed
  application:set_env(dnsserver,tcp_port,8888),

  ct:log(sys_state, "Starting the supervisor~n"),
  {ok, SuperPid} = tcp_server_sup:start_link(),
  [{supervisor, SuperPid}| Config].

end_per_testcase(_TestCase, Config) ->
  ct:log(sys_state, "Stopping the supervisor~n"),

  %% interesting read: http://erlang.org/pipermail/erlang-questions/2010-June/051869.html
  %% so unlink the process as the supervisor is linked
  %% to the CT process
  SuperPid = ?config(supervisor, Config),
  unlink(SuperPid),
  exit(SuperPid, normal).

%% Test ipv4 and ipv6 connectivity
tcp_test(_Config) ->
  {ok, Port} = application:get_env(dnsserver,tcp_port),

  %% ipv4 connectivity
  {ok, Socket1} = gen_tcp:connect("127.0.0.1", Port, [inet,{active,true}]),
  {ok,{{127,0,0,1},_}} = inet:sockname(Socket1),
  ok = gen_tcp:send(Socket1, "Some Data"),
  receive
    Any1  ->
      {tcp,_Socket1,"Some Data"}  = Any1
  after 1000 ->
    gen_tcp:close(Socket1),
    ct:fail("timeout")
  end,
  ok = gen_tcp:close(Socket1),

  %% ipv6 connectivity
  {ok, Socket2} = gen_tcp:connect("::1", Port, [inet6,{active,true}]),
  {ok,{{0,0,0,0,0,0,0,1},_}} = inet:sockname(Socket2),
  ok = gen_tcp:send(Socket2, "Some Data"),
  receive
    Any2  ->
      {tcp,_Socket2,"Some Data"}  = Any2
  after 1000 ->
    gen_tcp:close(Socket2),
    ct:fail("timeout")
  end,
  ok = gen_tcp:close(Socket2),
  ok.

%% Test the number of accepting sockets
child_test(_Config) ->
  {ok, _Port} = application:get_env(dnsserver,tcp_port),
  {ok, N} = application:get_env(dnsserver,tcp_nof_accept_sockets),
  {links,A1} = erlang:process_info(erlang:whereis(tcp_accept_server_sup),links),
  %% 2 extra processes, one for the ListenSocket and one for its own supervisor
  2 = length(A1) - N.
%% TODO: make this work stable ; seems to work in normal operation, but not in CT environment....
%% TODO USE supervisor:count_children?
%%  %% open a socket, that creates a new process
%%  {ok, Socket1} = gen_tcp:connect("::1", Port, [inet6,{active,true}]),
%%  {links,A2} = erlang:process_info(erlang:whereis(tcp_accept_server_sup),links),
%%  3 = length(A2) - N,
%%
%%  %% open another socket
%%  {ok, Socket2} = gen_tcp:connect("::1", Port, [inet6,{active,true}]),
%%  {links,A3} = erlang:process_info(erlang:whereis(tcp_accept_server_sup),links),
%%  4  = length(A3) - N,
%%
%%  %% close a socket
%%  ok = gen_tcp:close(Socket1),
%%  {links,A4} = erlang:process_info(erlang:whereis(tcp_accept_server_sup),links),
%%  3 = length(A4) - N,
%%
%%  %% close another socket
%%  ok = gen_tcp:close(Socket2),
%%  {links,A5} = erlang:process_info(erlang:whereis(tcp_accept_server_sup),links),
%%  2 = length(A5) - N.