%% @author bokner
%% @doc @todo Helper functions for geofilter app.
-module(geofilter).

-define(POOL_NAME, redis_pool).
-define(GEOHASH_KEY(H), ["geonum:", integer_to_list(H)]).
-define(USER_KEY(Id), ["geonum_user:", Id]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).
-export([bbox/1, neighbors/1, set/4, delete/1]).
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
  {ok, Bbox} = geohash:decode_bbox_int(Hash),
  Bbox.

neighbors(Hash) ->
  cmd(["ZRANGE", ?GEOHASH_KEY(Hash), 0, -1]).

set(UserId, Lat, Lon, Precision) ->
  %% Calculate geohash for immediate bounding box, and 8 surrounding boxes.
  Geohashes3x3 = hashes_3x3(Lat, Lon, Precision),
  Commands = lists:map(fun(H) -> ["ZADD", ?GEOHASH_KEY(H), ts(), UserId] end, Geohashes3x3),
  UserGeohash = hd(Geohashes3x3),
  StoreUserCommand = ["SET", ?USER_KEY(UserId), UserGeohash],
  spawn(fun() -> cmd([StoreUserCommand | Commands]) end),
  UserGeohash.

delete(UserId) ->
  case cmd(["GET", ?USER_KEY(UserId)]) of
	undefined ->
	  {error, not_found};
	Hash ->
	  HashInt = list_to_integer(binary_to_list(Hash)),
	  {ok, AdjacentHashes} = geohash:neighbors_int(HashInt),
	  Commands = lists:map(fun(H) -> ["ZREM", ?GEOHASH_KEY(H), UserId] end, [HashInt | AdjacentHashes]),
	  RemoveUserCommand = ["DEL", ?USER_KEY(UserId)],
	  spawn(fun() -> cmd([RemoveUserCommand | Commands]) end),
	  ok
  end.
  
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
