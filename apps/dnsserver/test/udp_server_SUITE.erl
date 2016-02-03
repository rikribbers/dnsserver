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
%%% UDP socket server test
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(udp_server_SUITE).
-author("rik.ribbers").

-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([ipv4_test/1, ipv6_test/1]).


all() ->
  [ipv4_test, ipv6_test].

init_per_testcase(_TestCase, Config) ->
  test_utils:set_environment_variables(),
  ct:log(sys_state, "Starting the supervisor~n"),
  {ok, SuperPid} = udp_server_sup:start_link(),
  [{supervisor, SuperPid}| Config].

end_per_testcase(_TestCase, Config) ->
  ct:log(sys_state, "Stopping the supervisor~n"),
  SuperPid = ?config(supervisor, Config),
  exit(SuperPid, normal),
  ok.

%% Test ipv4 connectivity
ipv4_test(_Config) ->
  {ok, Port} = application:get_env(dnsserver,udp_port),
  {ok,Socket} = gen_udp:open(0, [inet,{active,true}]),
  gen_udp:send(Socket,"127.0.0.1", Port,"Some Data"),
  receive
    Any  ->
      ct:log("Any=~p",[Any]),
      {udp,_Socket,_Host,_Port,"Some Data"}  = Any
  after 1000 ->
    gen_udp:close(Socket),
    ct:fail("timeout")
  end,
  ok = gen_udp:close(Socket),
  ok.

%% Test ipv6 connectivity
ipv6_test(_Config) ->
  {ok, Port} = application:get_env(dnsserver,udp_port),
  {ok,Socket} = gen_udp:open(0, [inet6,{active,true}]),
  gen_udp:send(Socket,"::1", Port,"Some Data"),
  receive
    Any  ->
      ct:log("Any=~p",[Any]),
      {udp,_Socket,_Host,_Port,"Some Data"}  = Any
  after 1000 ->
    gen_udp:close(Socket),
    ct:fail("timeout")
  end,
  ok = gen_udp:close(Socket),
  ok.

