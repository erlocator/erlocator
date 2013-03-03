%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for geofilter.

-module(geofilter_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/3]).


-compile(export_all).
%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
	AppParams = application:get_all_env(geofilter),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot, AppParams)
           end,
	%%
	spawn(fun() -> geofilter:start(AppParams) end),
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot, AppParams) ->
    "/" ++ Path = Req:get(path),	
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
		  Params = Req:parse_qs(),
            case Path of
              	"geo/neighbors" ->
				 Hash = list_to_integer(proplists:get_value("geonum", Params)), 
                 Req:ok({"application/json", [], 
						 to_json(geofilter:neighbors(Hash))});
			  "geo/bbox" ->
				Hash = list_to_integer(proplists:get_value("geonum", Params)),
				Req:ok({"application/json", [], 
						 to_json({bbox, geofilter:bbox(Hash)})});
                _ ->
                    %%Req:serve_file(Path, DocRoot)
                  Req:not_found()
            end;
        'POST' ->
		    Params = Req:parse_post(),
            case Path of
			  "geo/set" ->
				UserId = proplists:get_value("id", Params),
				Lat = proplists:get_value("lat", Params),
				Lon = proplists:get_value("lon", Params),
				UserHash = geofilter:set(UserId, to_number(Lat), to_number(Lon), proplists:get_value(geonum_bits, AppParams)),
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
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


to_number(Num) when is_number(Num) ->
  Num;

to_number(Str) ->
    case string:to_float(Str) of
        {error,no_float} -> list_to_integer(Str)*1.0;
        {F,_Rest} -> F
    end.

to_json(Neighbors) when is_list(Neighbors) ->
  json_utils:encode({struct, [{"neighbors", {array, Neighbors}}]});

to_json({geonum, _Hash} = Data) ->
  json_utils:encode({struct, [Data]});

to_json({bbox, Bbox}) ->
  {{TopLeftLat, TopLeftLon}, {BottomRightLat, BottomRightLon}} = Bbox,
  json_utils:encode({struct, [{"top_left", {struct, [{"lat", TopLeftLat}, {"lon", TopLeftLon}]}}, 
							  {"bottom_right", {struct, [{"lat", BottomRightLat}, {"lon", BottomRightLon}]}}
							 ]});
to_json(_) ->
  throw(unkonwn_json_type).

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
