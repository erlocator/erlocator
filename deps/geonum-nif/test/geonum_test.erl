%% @author bokner
%% @doc Test suite for geonum.

-module(geonum_test).
-include_lib("eunit/include/eunit.hrl").

geonum_encode_test() ->
  ?assertEqual({ok, 6002799572}, geonum:encode(40.689167, -74.044444, 32)),
  ?assertEqual({ok, 32}, geonum:precision(6002799572)).

geonum_decode_test() ->
  {ok, {Lat, Lon}} = geonum:decode(6002799572),
  ?assertEqual({ok, 6002799572}, geonum:encode(Lat, Lon, 32)).

geonum_decode_bbox_test() ->
  Lat = 40.689167, 
  Lon = -74.044444,
  {ok, GeoNum} = geonum:encode(Lat, Lon, 32),
  {ok, {TopLeft, BottomRight}} = geonum:decode_bbox(GeoNum),
  {TLat, TLon} = TopLeft,
  {BLat, BLon} = BottomRight,
  ?assert(TLat >= Lat andalso BLat =< Lat),
  ?assert(TLon =< Lon andalso BLon >= Lon).

geonum_neighbors1_test() ->
  ?assertEqual({ok,6002799573}, geonum:neighbor(6002799572, n)),
  ?assertEqual({ok,6002799486}, geonum:neighbor(6002799572, w)),
  ?assertEqual({ok,6002799569}, geonum:neighbor(6002799572, s)),
  ?assertEqual({ok,6002799574}, geonum:neighbor(6002799572, e)),
  ?assertError(badarg, geonum:neighbor(6002799572, nw)).

geonum_neighbors2_test() ->
  Geonum = 6002799572,
  {ok, N} = geonum:neighbor(Geonum, n),
  {ok, W} = geonum:neighbor(Geonum, w),
  {ok, S} = geonum:neighbor(Geonum, s),
  {ok, E} = geonum:neighbor(Geonum, e),
  
  {ok, NW} = geonum:neighbor(N, w),
  {ok, SE} = geonum:neighbor(S, e),
  {ok, NE} = geonum:neighbor(E, n),
  {ok, SW} = geonum:neighbor(W, s),
  {ok, Neighbors} = geonum:neighbors(Geonum),
  ?assertEqual([W, E, N, S, NW, NE, SW, SE], Neighbors),
  ?assertEqual(SW, lists:min(Neighbors)),
  ?assertEqual(NE, lists:max(Neighbors)).
  