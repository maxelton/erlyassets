-module(simple_assets_app).

-behaviour(application).

-include("../../include/erlyassets.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        %% {HostMatch, list({PathMatch, Handler, Opts})}
        {'_', [
            {"/", simple_assets_dtl_handler, []},
            {?ASSETDIR ++ "/:asset", erlyassets_cowboy_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    simple_assets_sup:start_link().

stop(_State) ->
    ok.
