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

-export([ipv4_test/1, ipv6_test/1]).


all() ->
  [ipv4_test, ipv6_test].

init_per_testcase(_TestCase, Config) ->
  set_environment_variables(),
  ct:log(sys_state, "Starting the supervisor~n"),
  {ok, SuperPid} = tcp_server_sup:start_link(),
  [{supervisor, SuperPid}| Config].

end_per_testcase(_TestCase, Config) ->
  ct:log(sys_state, "Stopping the supervisor~n"),
  SuperPid = ?config(supervisor, Config),
  exit(SuperPid, normal),
  Config.

ipv4_test(_Config) ->
  {ok, 8053} = application:get_env(dnsserver,tcp_port),
  {ok, Socket} = gen_tcp:connect("127.0.0.1", 8053, [inet,{active,true}]),
  ok = gen_tcp:send(Socket, "Some Data"),
  receive
    Any  ->
      {tcp,_Socket,"Some Data"}  = Any
  after 1000 ->
    ct:fail("timeout")
  end,
  ok = gen_tcp:close(Socket),
  ok.

ipv6_test(_Config) ->
  {ok, 8053} = application:get_env(dnsserver,tcp_port),
  {ok, Socket} = gen_tcp:connect("::1", 8053, [inet6,{active,true}]),
  ok = gen_tcp:send(Socket, "Some Data"),
  receive
    Any  ->
      {tcp,_Socket,"Some Data"}  = Any
  after 1000 ->
    ct:fail("timeout")
  end,
  ok = gen_tcp:close(Socket),
  ok.


set_environment_variables() ->
  % read config file
  ConfigFilePath = filename:join([filename:dirname(code:which(?MODULE)), "test.config"]),
  {ok, [AppsConfig]} = file:consult(ConfigFilePath),
  % loop to set variables
  F = fun({AppName, AppConfig}) ->
    set_environment_for_app(AppName, AppConfig)
      end,
  lists:foreach(F, AppsConfig).

set_environment_for_app(AppName, AppConfig) ->
  F = fun({Key, Val}) ->
    application:set_env(AppName, Key, Val)
      end,
  lists:foreach(F, AppConfig).
