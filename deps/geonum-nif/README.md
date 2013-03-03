GeoNum
=======

A NIF based geonum module implementation for Erlang.

GeoNum is an encoding of geolocation, similar to geohash (http://en.wikipedia.org/wiki/Geohash), the difference being that it's encoded as integer value, and allows more granular precision (by 1-bit, as opposed to geohash that uses 5-bit encoded characters; the maximum precision for geohash is 60 bits, whereas geonum allows 62 bits).
The implementation based on http://karussell.wordpress.com/2012/05/23/spatial-keys-memory-efficient-geohashes/, with some modifications. 


Example Usage
-------------

In the example, we encode lat/lon coordinates of the Statue of Liberty to the geonum value with 62-bit precision (1), then decode (2) (note the precision being preserved) and calculate a bounding box (3) of the area that
is represented by the geonum value. We can also request adjacent geonums in 3x3 extension area in order to use them for proximity searches (4) (more on it at http://en.wikipedia.org/wiki/Geohash#Limitations).  

```erlang
$ erl -pa ebin        
Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9.1  (abort with ^G)
1>{ok, Hash} = geonum:encode(40.689167, -74.044444, 62).
{ok,6445456962182867325}

2> geonum:decode(Hash).                                           
{ok,{40.68916704040021,-74.04444406740367}}

3> geonum:decode_bbox(Hash).      
{ok,{{40.68916708230972,-74.0444441512227},
     {40.68916699849069,-74.04444398358464}}}
4> geonum:neighbors(Hash).
   {ok,[6445456962182867319,6445456962182867327,
	     6445456962182868008,6445456962182867324,6445456962182868002,
	     6445456962182868010,6445456962182867318,
	     6445456962182867326]}




```
