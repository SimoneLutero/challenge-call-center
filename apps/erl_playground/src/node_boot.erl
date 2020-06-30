-module(node_boot).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]). -ignore_xref([{start_link, 0}]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    status = booting :: booting | fully_booted
}).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

% ?MODULE is name of this module (node_boot in this case)

-define(SERVER, ?MODULE).
-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    {ok, _} = gen_server:start_link({local, ?SERVER}, ?CB_MODULE, [], []).

% If name registration succeeds, the new gen_server process calls the callback function node_boot:init([])

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, Init} = application:get_env(erl_playground, boot_mode),
    boot(Init),

    {ok, #state {
        status = fully_booted
    }}.

% Synchronous requests

handle_call(_Request, _From, State) ->
    {noreply, State}.

% Asynchronous requests

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Definitions
%% ------------------------------------------------------------------

-spec boot(Init :: atom()) -> ok.
boot(client_server) ->
    sockserv:start(),
    sockclient:start_link(),
    ok;
boot(only_server) ->
    sockserv:start(),
    ok;
boot(only_client) ->
    sockclient:start_link(),
    ok;
boot(_) ->
    ok.
