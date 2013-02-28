{application, geofilter,
 [{description, "geofilter"},
  {vsn, "0.01"},
  {modules, [
    geofilter,
    geofilter_app,
    geofilter_sup,
    geofilter_web,
    geofilter_deps
  ]},
  {registered, []},
  {mod, {geofilter_app, []}},
  {env, [{geohash_bits, 25},
  			{redis_pool_size, 20},
  			{redis_host, "localhost"},
  			{redis_port, 6379}
  		]},
  {applications, [kernel, stdlib, crypto]}]}.
