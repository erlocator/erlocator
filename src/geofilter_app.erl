%%% -------------------------------------------------------------------
%%%
%%% Copyright	: 2011, The Big Live, Inc.
%%%
%%% -------------------------------------------------------------------

%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the geofilter application.

-module(geofilter_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for geofilter.
start(_Type, _StartArgs) ->
    geofilter_deps:ensure(),
    geofilter_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for geofilter.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
