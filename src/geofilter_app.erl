%% @author Boris Okner <boris.okner@gmail.com>
%% @author Josh Murphy <jmurphy@lostbitz.com>
%% @author Christian Gribneau <christian@gribneau.net>
%% @copyright 2013

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
