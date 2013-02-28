%% @author bokner
%% @doc @todo Helper functions for geofilter app.
-module(geofilter).

-define(POOL_NAME, redis_pool).
-define(GEOHASH_KEY(H), ["geohash:", integer_to_list(H)]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).
-export([bbox/1, neighbors/1, set/4]).
-export([start/0, stop/0]).

-compile(export_all).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the geofilter server.
start() ->
    geofilter_deps:ensure(),
    ensure_started(crypto),
    application:start(geofilter).

%% @spec stop() -> ok
%% @doc Stop the geofilter server.
stop() ->
    Res = application:stop(geofilter),
    application:stop(crypto),
    Res.


start(Opts) ->
  	PoolSize = proplists:get_value(redis_pool_size, Opts, 10),
    RedoOpts = [{host, proplists:get_value(redis_host, Opts, [])}, {port, proplists:get_value(redis_port, Opts, [])}],
    ChildMods = [redo, redo_redis_proto, redo_uri],
    ChildMFA = {redo, start_link, [undefined, RedoOpts]},

    supervisor:start_child(geofilter_sup,
                           {geofilter_redis_sup,
                            {cuesport, start_link,
                             [?POOL_NAME, PoolSize, ChildMods, ChildMFA]},
                            transient, 2000, supervisor, [cuesport | ChildMods]}).
bbox(Hash) ->
  todo, {bbox, {}}.

neighbors(Hash) ->
  cmd(["ZRANGE", ?GEOHASH_KEY(Hash), 0, -1]).

set(UserId, Lat, Lon, Precision) ->
  %% Calculate geohash for immediate bounding box, and 8 surrounding boxes.
  Geohashes3x3 = hashes_3x3(Lat, Lon, Precision),
  Commands = lists:map(fun(H) -> ["ZADD", ?GEOHASH_KEY(H), ts(), UserId] end, Geohashes3x3),
  spawn(fun() -> cmd(Commands) end),
  hd(Geohashes3x3).


%% ====================================================================
%% Internal functions
%% ====================================================================
-spec cmd(iolist()) -> iolist() | integer().
cmd(Cmd) ->
    redo:cmd(cuesport:get_worker(?POOL_NAME), Cmd).

hashes_3x3(Lat, Lon, Precision) ->
  {ok, H} = geohash:encode_int(Lat, Lon, Precision),
  {ok, NeighborHashes} = geohash:neighbors_int(H),
  [H | NeighborHashes].

%% Time in milliseconds
-spec ts() -> integer().
ts() ->
  {Mega, Sec, Micro} = now(),
  Mega * 1000000000 + Sec * 1000 + erlang:round(Micro / 1000).
