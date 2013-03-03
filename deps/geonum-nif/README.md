GeoNum
=======

A NIF based geohash module implementation for Erlang.

GeoNum is an encoding of geolocation, similar to geohash (http://en.wikipedia.org/wiki/Geohash), the difference being that it's encoded as integer value, and allows more granular precision (by 1-bit, as opposed to geohash that uses 5-bit encoded characters).
The implementation based on http://karussell.wordpress.com/2012/05/23/spatial-keys-memory-efficient-geohashes/, with some modifications. 


Example Usage
-------------

In the example, we encode lat/lon coordinates of the given point to the geonum value with 32-bit precision (1), then calculate midpoint (2) and bounding box (3) of the area that
is represented by the geonum value. We can also request adjacent geonums in 3x3 extension area in order to use them for proximity searches (more on it at http://en.wikipedia.org/wiki/Geohash#Limitations).  

```erlang
$ erl -pa ebin        
Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9.1  (abort with ^G)
1> {ok, Hash} = geonum:encode(12.2, 12.4, 32).
{ok,7541470019}
2> geonum:decode(Hash).
{ok,{12.198944091796875,12.40081787109375}}
3> geonum:decode_bbox(Hash).
{ok,{{12.2003173828125,12.3980712890625},
     {12.19757080078125,12.403564453125}}}
4> 	geonum:neighbors(Hash).      
{ok,[7541470017,7541470025,7541470022,7541470018,7541470020,
	     7541470028,7541470016,7541470024]}
```
