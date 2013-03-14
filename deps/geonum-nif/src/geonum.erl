%% Copyright (c) 2013, BigLive Inc.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @author Boris Okner <boris.okner@gmail.com>
%% @copyright 2012 BigLive Inc.
%% @doc GeoNum functions for Erlang with C implementations for core 
%% functionality
%% @end

-module(geonum).

-export([
    decode/1,
    decode_bbox/1,
    encode/3,
    neighbor/2,
    neighbors/1,
	precision/1
]).

-on_load(init/0).

%% @doc Decode a geonum in to latitude and longitude
-spec decode(pos_integer()) -> {float(), float()}.
decode(_GeoHash) ->
    exit(geonum_nif_not_loaded).

%% @doc Decode a  geonum into top left and bottom right points
-spec decode_bbox(pos_integer()) -> {{float(), float()}, {float(), float()}}.
decode_bbox(_GeoHash) ->
    exit(geonum_nif_not_loaded).

%% @doc Encode latitude and longitude into a geonum
-spec encode(float(), float(), pos_integer()) -> binary().
encode(_Latitude, _Longitude, _Precision) ->
    exit(geonum_nif_not_loaded).

%% @doc Calculate a neighboring geonum
-spec neighbor(binary(), n | s | w | e) -> binary().
neighbor(_GeoNum, _Direction) ->
    exit(geonum_nif_not_loaded).

%% @doc Get all neighbors of binary geonum
-spec neighbors(binary()) -> [binary()].
neighbors(_GeoNum) ->
    exit(geonum_nif_not_loaded).

%% @doc Get precision (number of significant bits) of binary geonum
-spec precision(binary()) -> [binary()].
precision(_GeoNum) ->
    exit(geonum_nif_not_loaded).

%% @private
init() ->
    SoName = case code:priv_dir(?MODULE) of
    {error, bad_name} ->
        case filelib:is_dir(filename:join(["..", "priv"])) of
        true ->
            filename:join(["..", "priv", "geonum_nif"]);
        false ->
            filename:join(["priv", "geonum_nif"])
        end;
    Dir ->
        filename:join(Dir, "geonum_nif")
    end,
    (catch erlang:load_nif(SoName, 0)),
    case erlang:system_info(otp_release) of
    "R13B03" -> true;
    _ -> ok
    end.
