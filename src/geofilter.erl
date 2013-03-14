%% @author Boris Okner <boris.okner@gmail.com>
%% @author Josh Murphy <jmurphy@lostbitz.com>
%% @author Christian Gribneau <christian@gribneau.net>
%% @copyright 2013
%% @todo Helper functions for geofilter app.

-module(geofilter).

-define(POOL_NAME, redis_pool).
-define(GEONUM_KEY(H), ["geonum:", integer_to_list(H)]).
-define(USER_KEY(Id), ["geonum_user:", Id]).
-define(DEFAULT_GEN_NUMBER, 100).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).
-export([bbox/1, bbox_3x3/1, neighbors/1, neighbors_full/1, set/5, delete/1]).
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

%% @doc Return bounding box coordinates for a single region.
bbox(Hash) ->
    {ok, Bbox} = geonum:decode_bbox(Hash),
    Bbox.

%% @doc Return bounding box coordinates for a 3x3 region.
bbox_3x3(Hash) ->
    Hashes = hashes3x3(Hash),
    %% Take top left coordinate of north-west box and bottom right coordinate of south-east box
    NW = lists:nth(6, Hashes),
    SE = lists:last(Hashes),
    {TopLeft, _} = bbox(NW),
    {_, BottomRight} = bbox(SE),
    {TopLeft, BottomRight}.

%% @doc Return list of neighbors for a given hash.
neighbors(Hash) ->
  Commands = lists:map(fun(H) ->
				["ZRANGE", ?GEONUM_KEY(H), 0, -1]
					   end, hashes3x3(Hash)),
    lists:flatten(cmd(Commands)).

%% @doc Return list of neighbors with associated data for a hash.
neighbors_full(Hash) ->
    case neighbors(Hash) of
        undefined ->
            [];
        UserIds ->	  
            Commands = lists:map(fun(UserId) -> ["GET", ?USER_KEY(UserId)] end, UserIds),
            case cmd(Commands) of
                Results when is_list(Results) ->
                    lists:foldl(fun(undefined, Acc) -> Acc;

                (R, Acc) ->
                    [binary_to_term(R) | Acc] 
                    end, [], Results);

                Results when is_binary(Results) ->
		  [binary_to_term(Results)]
	  end
  end.

%% @doc Create record for a user at a given location.
set(UserId, Lat, Lon, Precision, Options) ->
    %% Calculate geonum for immediate bounding box and 8 surrounding boxes.
    {ok, UserGeonum} = geonum:encode(Lat, Lon, Precision),
	set1(UserId, UserGeonum, Lat, Lon, Options),
	UserGeonum.

set1(UserId, Geonum, Lat, Lon, Options) ->								  
	%% Delete user first, in order to avoid erroneous records in geonum sets due to the change of user's location.
    geofilter:delete(UserId),
    Geonums3x3 = hashes3x3(Geonum),
    Commands = lists:map(fun(H) -> ["ZADD", ?GEONUM_KEY(H), float_to_list(distance(Lat, Lon, H)), UserId] end, Geonums3x3),
    StoreUserCommand = ["SET", ?USER_KEY(UserId), term_to_binary([{"geonum", Geonum} | proplists:delete("geonum", Options)])],
    spawn(fun() -> cmd([StoreUserCommand, ["ZADD", ?GEONUM_KEY(Geonum), float_to_list(distance(Lat, Lon, Geonum)), UserId]]) end).

%% @doc Remove records for a user.
delete(UserId) ->
    case cmd(["GET", ?USER_KEY(UserId)]) of
        undefined ->
            {error, not_found};
        Data ->
            HashInt = proplists:get_value("geonum", binary_to_term(Data)),
            RemoveUserCommand = ["DEL", ?USER_KEY(UserId)],
            spawn(fun() -> cmd([RemoveUserCommand, ["ZREM", ?GEONUM_KEY(HashInt), UserId]]) end),
            ok
    end.

%% Generate random records within given geonum area.
generate(GeoNum, undefined) ->
  generate(GeoNum, ?DEFAULT_GEN_NUMBER);

generate(Geonum, Number) when is_integer(Number) ->
  {{MaxLat, MinLon}, {MinLat, MaxLon}} = bbox_3x3(Geonum),
  random:seed(now()),
  Precision = geonum:precision(Geonum),
  lists:foreach(fun(_) ->
					 Lat = random:uniform() * (MaxLat - MinLat) + MinLat,
					 Lon = random:uniform() * (MaxLon - MinLon) + MinLon,
					 {ok, G} = geonum:encode(Lat, Lon, Precision),
					 add_neighbor("neighbor_" ++ integer_to_list(random:uniform(10000000)), G, Lat, Lon)
				end, lists:seq(1, Number)).
%% ====================================================================
%% Internal functions
%% ====================================================================
-spec cmd(iolist()) -> iolist() | integer().
cmd(Cmd) ->
    redo:cmd(cuesport:get_worker(?POOL_NAME), Cmd).

%% @doc Return list of hashes for 3x3 region.
hashes3x3(Hash) ->
    {ok, NeighborHashes} = geonum:neighbors(Hash),
    [Hash | NeighborHashes].

%% @doc Time in milliseconds
-spec ts() -> integer().
ts() ->
    {Mega, Sec, Micro} = now(),
    Mega * 1000000000 + Sec * 1000 + erlang:round(Micro / 1000).

%% @doc Distance from the point {Lat, Lon} to the center of bounding box defined by Hash
-spec distance(float(), float(), integer()) -> float(). 
distance(Lat, Lon, Hash) ->
    {ok, {MidPointLat, MidPointLon} = _MidPoint} = geonum:decode(Hash),
    distance(Lat, Lon, MidPointLat, MidPointLon).

%% @doc Distance between two points (Haversine)
%% Reference to original code:
%% http://pdincau.wordpress.com/2012/12/26/distance-between-two-points-on-earth-in-erlang-an-haversine-function-implementation/
%% @end
%% @todo  Move to NIF
-spec distance(float(), float(), float(), float()) -> float().
distance(Lat1, Lng1, Lat2, Lng2) ->
    Deg2rad = fun(Deg) -> math:pi()*Deg/180 end,
    [RLat1, RLng1, RLat2, RLng2] = [Deg2rad(Deg) || Deg <- [Lat1, Lng1, Lat2, Lng2]],

    DLon = RLng2 - RLng1,
    DLat = RLat2 - RLat1,
  
    A = math:pow(math:sin(DLat/2), 2) + math:cos(RLat1) * math:cos(RLat2) * math:pow(math:sin(DLon/2), 2),

    C = 2 * math:asin(math:sqrt(A)),
  
    %% suppose radius of Earth is 6372.8 km
    Km = 6372.8 * C,
    Km.
%%
%% Helper function for generating neighbors
%%
-spec add_neighbor(string(), integer(), float(), float()) -> any().		
add_neighbor(NeighborId, Geonum, Lat, Lon) ->
  set1(NeighborId, Geonum, Lat, Lon, [{"id", NeighborId}, {"lat", Lat}, {"lon", Lon}, {"first_name", NeighborId}, {"last_name", "neighbor"}]).
