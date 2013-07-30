
-module(simple_assets_dtl_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Iolist} = dtl_simple_assets:render(),
    {ok, Req2} = cowboy_req:reply(200, [], Iolist, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

