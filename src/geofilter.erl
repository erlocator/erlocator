%% @author Boris Okner <boris.okner@gmail.com>
%% @author Josh Murphy <jmurphy@lostbitz.com>
%% @author Christian Gribneau <christian@gribneau.net>
%% @copyright 2013
%% @doc Helper functions for geofilter app.

-module(geofilter).

-define(POOL_NAME, redis_pool).
-define(GEONUM_KEY(H), ["geonum:", integer_to_list(H)]).
-define(GEONUM_EXPIRE, "geonum_expire").
-define(USER_KEY(Id), ["geonum_user:", Id]).
-define(DEFAULT_GEN_NUMBER, 100).
-define(DEFAULT_EXPIRATION, 1800000). %% The default TTL for the session (30 min)
-define(DEFAULT_CLEANUP_INTERVAL, 300000). %% The default interval for the session cleanup process (5 min) 
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).
-export([bbox/1, bbox_3x3/1, neighbors/1, neighbors_full/1, set/5, delete/1]).
-export([generate/2]).
-export([cleanup_expired/1, flushall/0]).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @doc Start the geofilter application.
-spec start() -> ok.
start() ->
    geofilter_deps:ensure(),
    ensure_started(crypto),
    application:start(geofilter).


%% @doc Stop the geofilter application.
-spec stop() -> ok.
stop() ->
    Res = application:stop(geofilter),
    application:stop(crypto),
    Res.

%% @doc Intialization code (redis client, background processes and anything that is needed to support web app before entering the request handling loop). 
-spec start(list()) -> any().		
start(Opts) ->
    %% Start redis client
    PoolSize = proplists:get_value(redis_pool_size, Opts, 10),
    RedoOpts = [{host, proplists:get_value(redis_host, Opts, [])}, {port, proplists:get_value(redis_port, Opts, [])}],
    ChildMods = [redo, redo_redis_proto, redo_uri],
    ChildMFA = {redo, start_link, [undefined, RedoOpts]},
  
    spawn(fun() -> supervisor:start_child(geofilter_sup,
        {geofilter_redis_sup,
            {cuesport, start_link,
            [?POOL_NAME, PoolSize, ChildMods, ChildMFA]},
            transient, 2000, supervisor, [cuesport | ChildMods]})
    end),
    %% The background process to clean up expired records
    {ok, _TRef} = timer:apply_interval(proplists:get_value(cleanup_interval, Opts, ?DEFAULT_CLEANUP_INTERVAL),
        geofilter, cleanup_expired, [proplists:get_value(expiration_interval, Opts, ?DEFAULT_EXPIRATION)]).

%% @doc Return bounding box coordinates for a single region.
-spec bbox(integer()) -> {tuple(), tuple()}.		
bbox(Hash) ->
    {ok, Bbox} = geonum:decode_bbox(Hash),
    Bbox.

%% @doc Return bounding box coordinates for a 3x3 region.
-spec bbox_3x3(integer()) -> {tuple(), tuple()}.
bbox_3x3(Hash) ->
    Hashes = hashes3x3(Hash),
    %% Take top left coordinate of north-west box and bottom right coordinate of south-east box
    NW = lists:nth(6, Hashes),
    SE = lists:last(Hashes),
    {TopLeft, _} = bbox(NW),
    {_, BottomRight} = bbox(SE),
    {TopLeft, BottomRight}.

%% @doc Return list of neighbors for a given hash.
-spec neighbors(integer()) -> list().
neighbors(Hash) ->
  Commands = lists:map(fun(H) ->
				["SMEMBERS", ?GEONUM_KEY(H)]
					   end, hashes3x3(Hash)),
    lists:flatten(cmd(Commands)).

%% @doc Return list of neighbors with associated data for a hash.
-spec neighbors_full(integer()) -> list().
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
-spec set(string(), float(), float(), integer(), list()) -> integer().
set(UserId, Lat, Lon, Precision, Options) ->
    %% Calculate geonum for immediate bounding box and 8 surrounding boxes.
    {ok, UserGeonum} = geonum:encode(Lat, Lon, Precision),
        set1(UserId, UserGeonum, Lat, Lon, Options),
        UserGeonum.

-spec set1(string(), integer(), float(), float(), list()) -> integer().

set1(UserId, Geonum, Lat, Lon, Options) ->
    %% Delete user first, in order to avoid erroneous records in geonum sets due to the change of user's location.
    geofilter:delete(UserId),
    StoreUserCommand = ["SET", ?USER_KEY(UserId), term_to_binary([{"geonum", Geonum} | proplists:delete("geonum", Options)])],
    spawn(fun() -> cmd([StoreUserCommand, 
						["SADD", ?GEONUM_KEY(Geonum), UserId],
					    ["ZADD", ?GEONUM_EXPIRE, ts(), UserId]
					   ]) end).

%% @doc Remove records for a user.
-spec delete(string()) -> ok | {error, not_found}.		
delete(UserId) ->
    case cmd(["GET", ?USER_KEY(UserId)]) of
        undefined ->
            {error, not_found};
        Data ->
            HashInt = proplists:get_value("geonum", binary_to_term(Data)),
            RemoveUserCommand = ["DEL", ?USER_KEY(UserId)],
            spawn(fun() -> cmd([RemoveUserCommand, 
								["SREM", ?GEONUM_KEY(HashInt), UserId],
								["ZREM", ?GEONUM_EXPIRE, UserId]
							   ]) end),
            ok
    end.

%% @doc Clean up expired records
-spec cleanup_expired(integer()) -> ok.
cleanup_expired(ExpirationInterval) ->
    io:format("Cleanup started...~n"),
    Expired = cmd(["ZRANGEBYSCORE", ?GEONUM_EXPIRE, 0, ts() - ExpirationInterval]),
    lists:foreach(fun(User) ->
        geofilter:delete(User)
        end, Expired).

%% @doc Wipe out all records.
-spec flushall() -> any().		
flushall() ->
    cmd(["FLUSHALL"]).

%% @doc Generate random records within given geonum area.
-spec generate(integer(), integer() | undefined) -> ok.
generate(GeoNum, undefined) ->
    generate(GeoNum, ?DEFAULT_GEN_NUMBER);

generate(Geonum, Number) when is_integer(Number) ->
    {{MaxLat, MinLon}, {MinLat, MaxLon}} = bbox_3x3(Geonum),
    random:seed(now()),
    {ok, Precision} = geonum:precision(Geonum),
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

%%
%% Helper function for generating neighbors
%%
-spec add_neighbor(string(), integer(), float(), float()) -> any().		
add_neighbor(NeighborId, Geonum, Lat, Lon) ->
    set1(NeighborId, Geonum, Lat, Lon, [{"id", NeighborId}, {"lat", Lat}, {"lon", Lon}, {"first_name", NeighborId}, {"last_name", "neighbor"}]).

%% Time in milliseconds
-spec ts() -> integer().
ts() ->
    {Mega, Sec, Micro} = now(),
    Mega * 1000000000 + Sec * 1000 + erlang:round(Micro / 1000).
