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
%%% @doc Supervisor for the UDP server
%%% @end
%%%-------------------------------------------------------------------
-module(udp_sup).
-author("rik.ribbers").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
    lager:debug("Starting udp_sup..."),
    UdpServer = {udp_server, {udp_server, start_link, []}, permanent, 30000, worker, [udp_server]},

    %% Get the module for handling data from the configuration.
    {ok, {Module, _Function,_Arity}} = application:get_env(udp,udp_function),

    Children = [UdpServer],
    RestartStrategy = {one_for_all, 5, 3600},

    %% check if process is already running, if so assume it is supervised
    %% No need to start it here then.
    case whereis(Module) of
        undefined ->
            DataHandler = {Module, {Module, start_link, []},
                permanent, 30000, worker, [Module]},
            {ok, {RestartStrategy, lists:append(Children,[DataHandler])}};
        _ ->
            lager:info("Already started Module=~p",[Module]),
            {ok, {RestartStrategy, Children}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
