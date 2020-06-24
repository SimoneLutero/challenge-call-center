-module(erl_playground_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Restart), {I, {I, start_link, []}, Restart, 5000, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->

    MaxR = 1000, % how many times
    MaxT = 10, % in how many seconds

    % SupFlags = #{strategy => one_for_all,
    %              intensity => MaxR,
    %              period => MaxT},

    % ChildSpecs = 
    %   [# {id => ranch_sup,
    %       start => {I, start_link, []},
    %       restart => permanent,
    %       shutdown => 5000,
    %       type => supervisor,
    %       modules => [ranch_sup]},
    %   # {id => node_boot,
    %       start => {I, start_link, []},
    %       restart => transient,
    %       shutdown => 5000,
    %       type => worker,
    %       modules => [node_boot]}
    %   ].

    % {ok, {SupFlags, ChildSpecs}}.

    % This supervisor is going to start ranch_sup to make possible to use ranch listeners
    % then it starts node_boot which starts server and/or client.

    {ok, { {one_for_all, MaxR, MaxT}, [
    	?CHILD(ranch_sup, supervisor),
        ?CHILD(node_boot, worker, transient)
    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
