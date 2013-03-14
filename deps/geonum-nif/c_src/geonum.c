/*
 * Copyright (c) 2013 Boris Okner, BigLive Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * 'Software'), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "erl_nif_compat.h"

#define GEONUM_MAX 62

#define min(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a < _b ? _a : _b; })

/*
 * Helper function to turn bit arrays to intervals
 */
void bits_to_interval(long long bits, long precision, double start, double end, double *res) {
    long long mask = (long long) 1 << (precision - 1);
    double r;
    while (mask != 0) {
        r = (end - start)/2 + start;
        if ((bits&mask) == mask) { // Right side of the interval
            start = r;
        } else { // Left side of the interval
            end = r;
        }
        mask = mask >> 1;
    }
    res[0] = start;
    res[1] = end;
}


void geonum_split(long long geonum, long long * lon_part, long long * lat_part) {
    long long oddBits = 0;
    long long evenBits = 0;
    int is_even = 1;
    int evenBitsNumber = 0, oddBitsNumber = 0;
    // Split lat and lon
    while (geonum != 1) {
        // retrieve rightmost bit from geonum
        int next_bit = geonum & 1;
        if (is_even) {
            evenBits = evenBits + ((long long) 1 << evenBitsNumber) * next_bit;
            evenBitsNumber++;
        } else {
            oddBits = oddBits + ((long long) 1 << oddBitsNumber) * next_bit;
            oddBitsNumber++;
        }
        geonum = geonum >> 1;
        is_even = !is_even;
    }
    // If the number of significant bits is even, then 'even' bits are for latitude, and 'odd' bits are for longitude
    if (is_even) {
        lon_part[0] = oddBits;
        lon_part[1] = oddBitsNumber;

        lat_part[0] = evenBits;
        lat_part[1] = evenBitsNumber;
    } else {
        lon_part[0] = evenBits;
        lon_part[1] = evenBitsNumber;

        lat_part[0] = oddBits;
        lat_part[1] = oddBitsNumber;
    }

}

void geonum_merge(long long lonBits, long long lonN, long long latBits, long long latN, long long * geonum) {
    long long h = 0;
    long long firstBits = 0, secondBits = 0;
    if (lonN > latN) {
        firstBits = lonBits;
        secondBits = latBits;
    } else {
        firstBits = latBits;
        secondBits = lonBits;
    }

    int i;
    for (i = 1; i <= lonN; i++) {
        int m = (i-1)*2;
        h = h + ((long long)1 << m) * (firstBits & 1);
        firstBits = firstBits >> 1;
        h = h + ((long long)1 << (m+1)) * (secondBits & 1);
        secondBits = secondBits >> 1;
    }
    geonum[0] = h + ((long long)1 << (lonN + latN));
}

void geonum_encode(double latitude, double longitude, int precision, long long * geonum)
{
    int is_even=1, i=0;
    double lat[2], lon[2], mid;
    long long ch = 0;

    lat[0] = -90.0;  lat[1] = 90.0;
    lon[0] = -180.0; lon[1] = 180.0;

    while (i < precision) {
        ch = ch<<1;
        if (is_even) {
            mid = (lon[0] + lon[1]) / 2;
            if (longitude > mid) {
                ch++;
                lon[0] = mid;
            } else
                lon[1] = mid;
        } else {
            mid = (lat[0] + lat[1]) / 2;
            if (latitude > mid) {
                ch++;
                lat[0] = mid;
            } else
                lat[1] = mid;
        }

        is_even = !is_even;
        i++;
    }
    // Cap the hash value with MSB = 1
    geonum[0] = ch + ((long long)1 << precision);
}



/*
 * Calculate bounding box for binary geonum
 */
void geonum_decode_bbox(long long geonum, double *top_left, double *bottom_right) {
    long long lon[2], lat[2];
    geonum_split(geonum, lon, lat);
    double lon_res[2], lat_res[2];
    bits_to_interval(lon[0], lon[1], -180, 180, lon_res);
    bits_to_interval(lat[0], lat[1], -90, 90, lat_res);
    top_left[0] = lat_res[1];
    top_left[1] = lon_res[0];
    bottom_right[0] = lat_res[0];
    bottom_right[1] = lon_res[1];
}


/**
 * Decode the mid point of the bounding box represented by a geonum
 */
static void
geonum_decode(long long geonum, double * point)
{
    double top[2], bottom[2];

    geonum_decode_bbox(geonum, top, bottom);

    point[0] = (top[0] + bottom[0]) / 2;
    point[1] = (top[1] + bottom[1]) / 2;
}

/**
 * Given the geonum value, find the bit precision
 */
void geonum_precision(long long geonum, int * precision) {
	int p = 0;
	int i;
	for (i = 0; i < GEONUM_MAX; i++) {
		if (geonum == 0) {
			p = -1;
			break;
		}
		if (geonum == 1) {
			break;
		} else {
			geonum = geonum >> 1;
			p++;
		}
	}
	precision[0] = p;
}

void geonum_neighbor(long long geonum, int direction, long long * neighbor) {
    // 0 - w, 1 - e, 2 - n, 3 - s
    long long lon[2], lat[2];

    geonum_split(geonum, lon, lat);
    switch (direction) {
        case 0:
            lon[0]--;
            break;
        case 1:
            lon[0]++;
            break;
        case 2:
            lat[0]++;
            break;
        case 3:
            lat[0]--;
            break;
        default:
            ;
    }
    geonum_merge(lon[0], lon[1], lat[0], lat[1], neighbor);

}


/**
 * Calculate all neighbors to a geonum of the same precision
 */
void
geonum_all_neighbors(long long geonum, long long * neighbors) {
    // 0 - w, 1 - e, 2 - n, 3 - s
    long long lon[2], lat[2];
    long long neighbor[1];

    geonum_split(geonum, lon, lat);
    // west
    geonum_merge(lon[0]-1, lon[1], lat[0], lat[1], neighbor);
    neighbors[0] = neighbor[0];
    // east
    geonum_merge(lon[0]+1, lon[1], lat[0], lat[1], neighbor);
    neighbors[1] = neighbor[0];
    // north
    geonum_merge(lon[0], lon[1], lat[0]+1, lat[1], neighbor);
    neighbors[2] = neighbor[0];
    // south
    geonum_merge(lon[0], lon[1], lat[0]-1, lat[1], neighbor);
    neighbors[3] = neighbor[0];
    // north-west
    geonum_merge(lon[0]-1, lon[1], lat[0]+1, lat[1], neighbor);
    neighbors[4] = neighbor[0];
    // north-east
    geonum_merge(lon[0]+1, lon[1], lat[0]+1, lat[1], neighbor);
    neighbors[5] = neighbor[0];
    // south-west
    geonum_merge(lon[0]-1, lon[1], lat[0]-1, lat[1], neighbor);
    neighbors[6] = neighbor[0];
    // south-east
    geonum_merge(lon[0]+1, lon[1], lat[0]-1, lat[1], neighbor);
    neighbors[7] = neighbor[0];

}


/**
 * Helper to make a valid erlang atom
 */
static inline ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom_compat(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

/**
 * Helper to make a valid erlang tuple containing {ok, Msg}
 */
static inline ERL_NIF_TERM
make_ok(ErlNifEnv* env, ERL_NIF_TERM mesg)
{
    ERL_NIF_TERM ok = make_atom(env, "ok");
    return enif_make_tuple2(env, ok, mesg);   
}

/**
 * Helper to make a valid erlang tuple containing {error, Msg}
 */
static inline ERL_NIF_TERM
make_error(ErlNifEnv* env, const char* mesg)
{
    ERL_NIF_TERM error = make_atom(env, "error");
    return enif_make_tuple2(env, error, make_atom(env, mesg));
}


/**
 * Erlang Wrapper for geonum_decode_bbox
 */
ERL_NIF_TERM
erl_geonum_decode_bbox(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double tl[2], br[2];
    long long geonum;

    if(!enif_get_int64(env, argv[0], &geonum)) {
            return enif_make_badarg(env);
        }

    geonum_decode_bbox(geonum, tl, br);

    ERL_NIF_TERM top_left = enif_make_tuple2(env,
            enif_make_double(env, tl[0]),
            enif_make_double(env, tl[1]));
    ERL_NIF_TERM bottom_right = enif_make_tuple2(env,
            enif_make_double(env, br[0]),
            enif_make_double(env, br[1]));
    ERL_NIF_TERM bbox = enif_make_tuple2(env, top_left, bottom_right);

    return make_ok(env, bbox);
}


/**
 * Erlang Wrapper for geonum_encode
 */
ERL_NIF_TERM
erl_geonum_encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double lat;
    double lon;
    int precision;
    long long bin_geonum[1];

    if(!enif_get_double(env, argv[0], &lat)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_double(env, argv[1], &lon)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int(env, argv[2], &precision)) {
        return enif_make_badarg(env);
    }

    if(precision >= GEONUM_MAX || precision < 1) {
        return make_error(env, "precision_range");
    }

    geonum_encode(lat, lon, precision, bin_geonum);

    return make_ok(env, enif_make_int64(env, bin_geonum[0]));
}

/**
 * Erlang Wrapper for geonum_decode
 */
ERL_NIF_TERM
erl_geonum_decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double point[2];
    long long geonum;

    if(!enif_get_int64(env, argv[0], &geonum)) {
        return enif_make_badarg(env);
    }

    geonum_decode(geonum, point);

    ERL_NIF_TERM point_tuple = enif_make_tuple2(env,
            enif_make_double(env, point[0]),
            enif_make_double(env, point[1]));

    return make_ok(env, point_tuple);
}

/**
 * Erlang Wrapper for geonum_precision
 */
ERL_NIF_TERM
erl_geonum_precision(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int precision[1];
    long long geonum;

    if(!enif_get_int64(env, argv[0], &geonum)) {
        return enif_make_badarg(env);
    }

    geonum_precision(geonum, precision);

    return make_ok(env, enif_make_int(env, precision[0]));
}


/**
 * Erlang Wrapper for  geonum_neighbor
 */
ERL_NIF_TERM
erl_geonum_neighbor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long long bin_neighbor[1];
    long long geonum;
    char dir[2];

    int dir_val;
    unsigned int dir_len;

    if(!enif_get_int64(env, argv[0], &geonum)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_atom_length(env, argv[1], &dir_len, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    if(dir_len > sizeof(dir)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_atom(env, argv[1], dir, sizeof(dir), ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    switch (dir[0]) {
        case 'w':
            dir_val = 0;
            break;
        case 'e':
            dir_val = 1;
            break;
        case 'n':
            dir_val = 2;
            break;
        case 's':
            dir_val = 3;
            break;
        default:
            return enif_make_badarg(env);
    }


    geonum_neighbor(geonum, dir_val, bin_neighbor);

    return make_ok(env, enif_make_int64(env, bin_neighbor[0]));
}

/**
 * Erlang Wrapper for binary geonum_neighbor
 */
ERL_NIF_TERM
erl_geonum_all_neighbors(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long long neighbors[8];
    long long geonum;

    if(!enif_get_int64(env, argv[0], &geonum)) {
        return enif_make_badarg(env);
    }

    geonum_all_neighbors(geonum, neighbors);

    return make_ok(env, enif_make_list8(env,
    		enif_make_int64(env, neighbors[0]),
    		enif_make_int64(env, neighbors[1]),
    		enif_make_int64(env, neighbors[2]),
    		enif_make_int64(env, neighbors[3]),
    		enif_make_int64(env, neighbors[4]),
    		enif_make_int64(env, neighbors[5]),
    		enif_make_int64(env, neighbors[6]),
    		enif_make_int64(env, neighbors[7])
    ));
}


int
on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}


int
on_reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}


int
on_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return 0;
}

static ErlNifFunc nif_functions[] = {
    {"encode", 3, erl_geonum_encode},
    {"decode", 1, erl_geonum_decode},
    {"neighbor", 2, erl_geonum_neighbor},
    {"neighbors", 1, erl_geonum_all_neighbors},
    {"decode_bbox", 1, erl_geonum_decode_bbox},
    {"precision", 1, erl_geonum_precision}
};

ERL_NIF_INIT(geonum, nif_functions, &on_load, &on_reload, &on_upgrade, NULL);
