
-module(erlyassets_cowboy_handler).

-behaviour(cowboy_http_handler).

-include("erlyassets.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

% -compile(export_all).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    ?DPRINT(["asset_handler called: ",Req,State]),
    {IMS,_} = cowboy_req:header(<<"if-modified-since">>,Req),
    {Asset,_} = cowboy_req:binding(asset,Req),
    {Reqmd5,_} = cowboy_req:qs(Req),
    [Etsdata] = ets:lookup(erlyassets_ets_table,Asset),
    {ok, Req2} = reply_http_request(Req,IMS,Reqmd5,Etsdata),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

% private
reply_http_request(Req,IMS,Reqmd5,Etsdata) when Reqmd5 == Etsdata#cachefile.md5 andalso IMS == Etsdata#cachefile.lastmodrfc1123 -> 
    reply_304_with_no_data(Req,Etsdata);
reply_http_request(Req,_IMS,Reqmd5,Etsdata) when Reqmd5 == Etsdata#cachefile.md5 -> 
    reply_200_with_data(Req,Etsdata);
reply_http_request(_Req,_IMS,_Reqmd5,_Etsdata) -> 
    {error, bad_md5}.

reply_304_with_no_data(Req,_Etsdata)->
    cowboy_req:reply(304, [], [], Req).

reply_200_with_data(Req,Etsdata)->
    {{Y,M,D},T} = erlang:localtime(),
    Expiretime = {{Y+1,M,D},T},
    cowboy_req:reply(200, [{<<"expires">>, cowboy_clock:rfc1123(Expiretime)},
                           {<<"last-modified">>, Etsdata#cachefile.lastmodrfc1123},
                           {<<"cache-control">>, << "public" >>}], Etsdata#cachefile.cachedata, Req).

