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
	Params = Req:parse_qs(),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
              	"geo/neighbors" ->
				 UserId = proplists:get_value("id", Params), 
                 Req:ok({"application/json", [], 
						 to_json(geofilter:neighbors(UserId, proplists:get_value(geohash_bits, AppParams)))});
			  "geo/bbox" ->
				UserId = proplists:get_value("id", Params),
				Req:ok({"application/json", [], 
						 to_json(geofilter:bbox(UserId, proplists:get_value(geohash_bits, AppParams)))});
                _ ->
                    %%Req:serve_file(Path, DocRoot)
                  Req:not_found()
            end;
        'POST' ->
            case Path of
			  "geo/set" ->
				UserId = proplists:get_value("id", Params),
				Lat = proplists:get_value("lat", Params),
				Lon = proplists:get_value("lon", Params),
				geofilter:set(UserId, to_number(Lat), to_number(Lon), proplists:get_value(geohash_bits, AppParams)),
				Req:respond({200,
                       [{"Content-Type", "text/html"}],
                       list_to_binary("OK")});
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


to_number(Str) ->
    case string:to_float(Str) of
        {error,no_float} -> list_to_integer(Str);
        {F,_Rest} -> F
    end.

to_json(Neighbors) when is_list(Neighbors) ->
  todo, "{\"neighbors\": []}";

to_json(Bbox) when is_tuple(Bbox) ->
  todo, "{\"bbox\": {}}";

to_json(_) ->
  throw(unkonwn_json_type).

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
