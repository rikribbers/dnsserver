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
%%% A generic resource discovery server based on the OTP in action
%%% book chapter 8 (ISBN: 9781933988788). Comments here are mostly
%%% directly copied from the book.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(resource_discovery).
-author("rik.ribbers").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  stop_link/0,
  add_target_resource_type/1,
  add_local_resource/2,
  fetch_resources/1,
  trade_resources/0
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).


%% It (the state record red.) has three fields: target_resource_types
%% is the “I want” part. This is a list of resource types that you’re
%% looking for. local_resource_tuples is the “I have” part. This is
%% where you list, in the form of resource tuples, all resources that
%% exist on the local node. Finally, found_resource_tuples is the
%% place where you cache information about discovered resource
%% instances matching your wanted list (even if they reside on the
%% local node).
-record(state, {target_resource_types,
  local_resource_tuples,
  found_resource_tuples}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  lager:info("Starting resource_discovery..."),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop_link() -> ok).
stop_link() ->
  gen_server:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Add the "I want" part
%%
%% @end
%%--------------------------------------------------------------------
add_target_resource_type(Type) ->
  gen_server:cast(?SERVER, {add_target_resource_type, Type}).

%%--------------------------------------------------------------------
%% @doc
%% Add the "I have" part
%%
%% @end
%%--------------------------------------------------------------------
add_local_resource(Type, Instance) ->
  gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

%%--------------------------------------------------------------------
%% @doc
%% This function is a synchronous call, asking for a list of all the
%% resource instances you’re looking for and know about for a given
%% resource type
%%
%% @end
%%--------------------------------------------------------------------
fetch_resources(Type) ->
  gen_server:call(?SERVER, {fetch_resources, Type}).

%%--------------------------------------------------------------------
%% @doc
%% Trade resources with other nodes.
%%
%% @end
%%--------------------------------------------------------------------
trade_resources() ->

  lager:info("Waiting for registration of local resources"),
  {ok,Time} = application:get_env(cluster,resource_discovery_wait_time),
  timer:sleep(Time),

  gen_server:cast(?SERVER, trade_resources),

  lager:info("Waiting for nodes to trade resources"),
  timer:sleep(Time),
  lager:info("Nodes=~p", [nodes()]).

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
init([]) ->
  {ok, #state{target_resource_types = [],
    local_resource_tuples = dict:new(),
    found_resource_tuples = dict:new()}}.

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
%% You call dict:find/2 to look up Type in the current resources. This
%% yields either {ok, Value} or error, which happens to be what you
%% want the fetch_resources/1 function to return, so there’s no need
%% to massage the result you can pass it straight back.
handle_call({fetch_resources, Type}, _From, State) ->
  lager:debug("handle_call {fetch_resources, ~p} State=~p ", [Type, State]),
  {reply, dict:find(Type, State#state.found_resource_tuples), State}.

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

handle_cast(stop, State) ->
  {stop, normal, State};

%% First, you pick out the current target resource types from the
%% server state. Then, you take the resource type you’ve been handed
%% and prepend it to the current list, first per- forming a delete so
%% that you avoid duplicate entries. (The lists:delete/2 function
%% leaves the list unchanged if it finds no such element.)
handle_cast({add_target_resource_type, Type}, State) ->
  TargetTypes = State#state.target_resource_types,
  NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
  lager:debug("handle_call {add_target_resource_type, ~p} State=~p TargetTypes=~p NewTargetTypes=~p", [Type, State, TargetTypes, NewTargetTypes]),
  {noreply, State#state{target_resource_types = NewTargetTypes}};

%% After picking out the current local resources, you store the new
%% resource instance under the specified type, and you put the bulk
%% of this operation in an internal utility function add_resource/3
handle_cast({add_local_resource, {Type, Instance}}, State) ->
  ResourceTuples = State#state.local_resource_tuples,
  NewResourceTuples = add_resource(Type, Instance, ResourceTuples),
  lager:debug("handle_call {add_local_resource, ~p} State=~p ResourceTuples=~p NewResourceTuples=~p", [Type, State, ResourceTuples, NewResourceTuples]),
  {noreply, State#state{local_resource_tuples = NewResourceTuples}};

%% The trade_resources message tells the local resource discovery
%% server to broadcast messages asynchronously to each of the resource
%% discovery servers on all the con- nected nodes in the Erlang cluster
%% (including the local node itself, for a nice symme- try that lets
%% you update your local list of matching resources without any
%% additional code). This is made simple by the fact that the processes
%% are all registered under the same name on their respective nodes.
handle_cast(trade_resources, State) ->
  ResourceTuples = State#state.local_resource_tuples,
  AllNodes = [node() | nodes()],
  lager:debug("Start trading resources with AllNodes=~p ResourceTuples=~p State=~p", [AllNodes, ResourceTuples, State]),
  lists:foreach(
    fun(Node) ->
      gen_server:cast({?SERVER, Node},
        {trade_resources, {node(), ResourceTuples}})
    end,
    AllNodes),

  {noreply, State};

%% The trade_resources message tells the local resource discovery
%% server to broadcast messages asynchronously to each of the resource
%% discovery servers on all the con- nected nodes in the Erlang cluster
%% (including the local node itself, for a nice symme- try that lets
%% you update your local list of matching resources without any
%% additional code). This is made simple by the fact that the processes
%% are all registered under the same name on their respective nodes.

%% These broadcast messages have the form {trade_resources, {ReplyTo,
%% Resources}}, where ReplyTo is the node name of the sender (given by
%% node()), and Resources is the entire data structure (a dict) that
%% holds the current resource tuples that the sender is publishing. Note
%% that you don’t need to worry about the receiving process mucking up
%% your local data structure, because message passing is strictly by
%% copy, and because Erlang allows you to send any data in
%% messages—there’s no need to rewrite or marshal the data—you can
%% include the dictionary as it is in the message.

%% First, you need a bunch of different fields from the current state,
%% so you use a slightly complicated pattern to extract these
%% immediately in the clause head. Note that the pattern has the shape
%% #state{...}=State, which means it’s an alias pattern: it both
%% matches and assigns a name at the same time. It’s common style to
%% write the name on the right side of the equals sign, because doing
%% so puts the visual focus on the shape of the data, but the pattern
%% can also be written State=#state{...}.
%% Next, you check to see if any of the sender’s resources are on your
%% “I want” list. Those whose types match are then added to your local
%% set of known resources. After that, you only have to reply to the
%% sender (the resource discovery process on the node identified by
%% the ReplyTo field). The reply has the same shape as the broadcast
%% mes- sage, but instead of the sender’s node name it uses the atom
%% noreply to indicate that no further reply is needed—otherwise,
%% messages would bounce back and forth for- ever. After the process
%% that broadcast the original trade messages has received and handled
%% all replies, it has the same information as the others.
handle_cast({trade_resources, {ReplyTo, Remotes}},
    #state{local_resource_tuples = Locals,
      target_resource_types = TargetTypes,
      found_resource_tuples = OldFound} = State) ->

  FilteredRemotes = resources_for_types(TargetTypes, Remotes),
  NewFound = add_resources(FilteredRemotes, OldFound),
  lager:debug("FilteredRemotes=~p NewFound=~p",[FilteredRemotes,NewFound]),
  case ReplyTo of
    noreply ->
      ok;
    _ ->
      gen_server:cast({?SERVER, ReplyTo},
        {trade_resources, {noreply, Locals}})
  end,
  {noreply, State#state{found_resource_tuples = NewFound}}.

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
handle_info(_Info, State) ->
  {noreply, State}.

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
terminate(_Reason, _State) ->
  lager:info("Stopping resource_discovery..."),
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
add_resource(Type, Resource, ResourceTuples) ->
  lager:debug("add_resource Type=~p Resource=~p", [Type, Resource]),
  case dict:find(Type, ResourceTuples) of
    {ok, ResourceList} ->
      NewList = [Resource | lists:delete(Resource, ResourceList)],
      lager:debug("ok NewList=~p", [NewList]),
      dict:store(Type, NewList, ResourceTuples);
    error ->
      lager:debug("Not found, adding new Resource"),
      dict:store(Type, [Resource], ResourceTuples)
  end.
add_resources([{Type, Resource} | T], ResourceTuples) ->
  add_resources(T, add_resource(Type, Resource, ResourceTuples));
add_resources([], ResourceTuples) ->
  ResourceTuples.

resources_for_types(Types, ResourceTuples) ->
  lager:debug("resources_for_types Types=~p", [Types]),
  Fun =
    fun(Type, Acc) ->
      case dict:find(Type, ResourceTuples) of
        {ok, List} ->
          [{Type, Instance} || Instance <- List] ++ Acc;
        error ->
          Acc
      end
    end,
  lists:foldl(Fun, [], Types).