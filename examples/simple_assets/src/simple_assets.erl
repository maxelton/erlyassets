
-module(simple_assets).

%% API.
-export([start/0]).
-export([stop/0]).
-export([restart/0]).

%% API.

start() ->
    erlydtl:compile("templates/simple_assets.dtl", dtl_simple_assets, [{custom_tags_modules,[erlyassets_dtl_tags]}]),
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(erlyassets),
    ok = application:start(simple_assets).

stop() ->
    application:stop(crypto),
    application:stop(ranch),
    application:stop(cowboy),
    application:stop(erlyassets),
    application:stop(simple_assets).

restart() ->
    stop(),
    start().

