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
%%% @doc cluster public API
%%% @end
%%%-------------------------------------------------------------------

-module('cluster_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1
        ,start_cluster/1
       ]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  lager:info("Starting application cluster..."),
  'cluster_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  lager:info("Stopping application cluster..."),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
%% Report yourself to the cluster for sharing resources
start_cluster(SharedResources) ->
  lager:debug("SharedResources=~p",[SharedResources]),

  %% Make sure the resource discovery worker is up-and-running
  %% and local mensia is not yet running.
  application:ensure_started(resource_discovery),
  stopped = mnesia:stop(),

  {ok, UseShadowServer} = application:get_env(cluster,use_shadow_servers),
  case UseShadowServer of
    true ->
      {ok, ShadowNodes} = application:get_env(cluster,shadow_servers),
      ok = ensure_contact_start(ShadowNodes);
    _ ->
      lager:info("Do not use shadow servers!")
  end,

  %% Add the shared resources
  lists:foreach( fun (R) -> resource_discovery:add_local_resource(R, node()),
    resource_discovery:add_target_resource_type(R) end,
    SharedResources),


  resource_discovery:trade_resources(),

  %% Wait for all nodes to answer and to settle things down.
  {ok,WaitTime} = application:get_env(cluster,resource_discovery_wait_time),
  timer:sleep(WaitTime),


  dynamic_db_init(SharedResources).


ensure_contact_start(ShadowNodes) ->
  lager:info("ShadowNodes=~p", [ShadowNodes]),
  case ShadowNodes of
    [] ->
      lager:error("no_shadow_nodes"),
      {error, no_shadow_nodes};
    ShadownNodes2 ->
      ensure_contact(ShadownNodes2)
  end.

ensure_contact(ShadowNodes) ->
  lager:debug("ShadowNodes=~p", [ShadowNodes]),
  Answering = [N || N <- ShadowNodes, net_adm:ping(N) =:= pong],
  case Answering of
    [] ->
      lager:error("no shadownodes anwsering ShadowNodes=~p", [ShadowNodes]),
      {error, no_contact_nodes_reachable};
    _ ->
      {ok, Time} = application:get_env(cluster,shadow_wait_time),
      wait_for_nodes(length(Answering), Time)
  end.

wait_for_nodes(MinNodes, WaitTime) ->
  lager:debug("MinNodes=~p WaitTime=~p", [MinNodes, WaitTime]),
  Slices = 10,
  SliceTime = round(WaitTime / Slices),
  wait_for_nodes(MinNodes, SliceTime, Slices).

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
  lager:debug("wait_for_node done!"),
  ok;

%% If that is the case, you assume you now are con- nected to all the nodes in the cluster. Otherwise, you go to
%% sleep for the duration of one time slice and repeat. If no other nodes turn up after the maximum wait time is up,
%% you go on anyway (presumably, it means you’re the first node in the cluster apart from the contact nodes).
wait_for_nodes(MinNodes, SliceTime, Iterations) ->
  lager:debug("wait_for_node MinNodes=~p SliceTime=~p Iterations=~p", [MinNodes, SliceTime, Iterations]),
  case length(nodes()) >= MinNodes of
    true ->
      ok;
    false ->
      timer:sleep(SliceTime),
      wait_for_nodes(MinNodes, SliceTime, Iterations - 1)
  end.

%% Start initialising database
dynamic_db_init([H|T]) ->
%%  N = [node()],
  {ok,E} = resource_discovery:fetch_resources(H),
  case E == [node()] of
    true ->
      lager:info("Initializing initial master mnesia node..."),
      %% Make sure mnesia is stopped before automagic resource
      %% discovery is started. mnesia will be started later.
      {ok, MnesiaDir} = application:get_env(cluster,mnesia_dir),
      application:set_env(mnesia, dir, MnesiaDir),
      ok = mnesia:start();
    false ->
      lager:info("Initializing new slave mnesia node..."),
      ok = mnesia:delete_schema([node()]),
      ok = mnesia:start(),
      %% Replaces local B schema wit remote C
      mnesia:add_table_copy(schema, node(), ram_copies),
      dynamic_db_init(H,T)
  end,
  application:ensure_started(mnesia),
  Tables = mnesia:system_info(tables),
  lager:debug("Tables=~p", [Tables]).


dynamic_db_init(Resource,[H|T]) ->
  {ok,Nodes} = resource_discovery:fetch_resources(Resource),
  add_extra_nodes(Nodes,Resource),
  dynamic_db_init(H,T);

dynamic_db_init(Resource,[]) ->
  {ok,Nodes} = resource_discovery:fetch_resources(Resource),
  add_extra_nodes(Nodes,Resource).

%% TODO -spec(add_extra_node(Nodes :: [node()] ,Resource :: any()) -> ok).
add_extra_nodes([Node | T],Resource) ->
  lager:info("Add extra node Node=~p T=~p Resource=~p", [Node, T,Resource]),
  case mnesia:change_config(extra_db_nodes, [Node]) of
    {ok, [Node]} ->
       mnesia:add_table_copy(Resource, node(), ram_copies),

      {ok,WaitTime} = application:get_env(cluster,tables_discovery_wait_time),
      mnesia:wait_for_tables(Resource, WaitTime);

    Response ->
      lager:error("Response=~p", [Response]),
      %%  Tries some other node instead
      add_extra_nodes(T,Resource)
  end;

add_extra_nodes([],_) ->
  ok.