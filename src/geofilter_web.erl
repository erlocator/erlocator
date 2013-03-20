%% @author Boris Okner <boris.okner@gmail.com>
%% @author Josh Murphy <jmurphy@lostbitz.com>
%% @author Christian Gribneau <christian@gribneau.net>
%% @copyright 2013


%% @doc Web server for geofilter.

-module(geofilter_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/3]).


-compile(export_all).

-define(FILE_REGEX, "^geonum/demo/").

%% External API

%% @doc Start the geofilter webserver.
start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
	AppParams = application:get_all_env(geofilter),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot, AppParams)
           end,
	%%
    geofilter:start(AppParams),
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

%% @doc Stop the geofilter webserver.
stop() ->
    mochiweb_http:stop(?MODULE).

%% @doc Handle requests to the geofilter webserver.
%% 
%% GET
%%
%%   geo/neighbors
%%
%%     Return neighbors near a specified location.
%%
%%   geo/bbox
%%
%%     Return neighbors near a specified location. 
%%
%%   Files
%%
%%     Return files comprising the demo application.
%% 
%%
%% POST
%% 
%%   geo/set
%%
%%     Set a user location.
%%
%%   geo/delete
%%
%%     Delete a user location.
%%
%%   geo/generate
%%
%%     Create n neighbors around a point.
%%
%% @end
loop(Req, DocRoot, AppParams) ->
    "/" ++ Path = Req:get(path),	
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            Params = Req:parse_qs(),
            case Path of
                "geo/neighbors" ->
                    Hash = list_to_integer(proplists:get_value("geonum", Params)), 
                        Req:ok({"application/json", [], 
                        to_json(geofilter:neighbors_full(Hash))});

                "geo/bbox" ->
                    Hash = list_to_integer(proplists:get_value("geonum", Params)),
                        Req:ok({"application/json", [], 
                        to_json({bbox, geofilter:bbox(Hash), geofilter:bbox_3x3(Hash)})});

                FilePath ->
                    case re:split(FilePath, ?FILE_REGEX) of
                        [_, FileName] ->
                            Req:serve_file(binary_to_list(FileName), DocRoot);

                        _Other ->
                            Req:not_found()
                    end
            end;

        'POST' ->
            Params = Req:parse_post(),
            case Path of
                "geo/set" ->
                    UserId = proplists:get_value("id", Params),
                    Lat = proplists:get_value("lat", Params),
                    Lon = proplists:get_value("lon", Params),
                    UserHash = geofilter:set(UserId, to_float(Lat), to_float(Lon), proplists:get_value(geonum_bits, AppParams), Params),
                    Req:respond({200,
                        [{"Content-Type", "application/json"}],
                        to_json({geonum, UserHash})});

                "geo/delete" ->
                    UserId = proplists:get_value("id", Params),
                    case geofilter:delete(UserId) of
                        ok ->
                            Req:respond({200, [], []});

                        {error, not_found} ->
                            Req:not_found();

                        _Other ->
                            Req:respond({500, [], []})
                    end;

                "geo/generate" ->
                    Geonum = proplists:get_value("geonum", Params),
                    Number = proplists:get_value("n", Params),
                    geofilter:generate(list_to_integer(Geonum), 
                        case Number of 
                            undefined ->
                                undefined;

                            N ->
                                list_to_integer(N)
                    end),
                    Req:respond({200, [], []});
			  "geo/flushall" ->
					geofilter:flushall(),
					Req:respond({200, [], []});										 
                _ ->
                    Req:not_found()
            end;

        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


to_float(Num) when is_float(Num) ->
    Num;

to_float(Str) ->
    case string:to_float(Str) of
        {error,no_float} -> list_to_integer(Str)*1.0;
        {F,_Rest} -> F
    end.

to_json(Neighbors) when is_list(Neighbors) ->
    mochijson:encode({struct, [{"neighbors", {array, lists:map(fun(N) -> {struct, N} end, Neighbors)}}]});

to_json({geonum, _Hash} = Data) ->
    mochijson:encode({struct, [Data]});

to_json({bbox, Bbox, Bbox3x3}) ->
    {{TopLeftLat, TopLeftLon}, {BottomRightLat, BottomRightLon}} = Bbox,
    {{TopLeftLat3x3, TopLeftLon3x3}, {BottomRightLat3x3, BottomRightLon3x3}} = Bbox3x3,
    mochijson:encode(
        {struct, [
            {"bbox", {struct, [
                {"top_left", {struct, [{"lat", TopLeftLat}, {"lon", TopLeftLon}]}}, 
                {"bottom_right", {struct, [{"lat", BottomRightLat}, {"lon", BottomRightLon}]}}
            ]}},
            {"bbox_3x3", {struct, [
                {"top_left", {struct, [{"lat", TopLeftLat3x3}, {"lon", TopLeftLon3x3}]}}, 
                {"bottom_right", {struct, [{"lat", BottomRightLat3x3}, {"lon", BottomRightLon3x3}]}}
            ]}}			  
        ]});

to_json(_) ->
    throw(unkonwn_json_type).

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
