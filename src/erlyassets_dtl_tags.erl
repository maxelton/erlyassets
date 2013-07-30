
-module(erlyassets_dtl_tags).

-include("erlyassets.hrl").

%% API.
-export([get_cache_url/1]).

% -compile(export_all).

get_cache_url(Tagvars) ->
    ?DPRINT(["get_cache_url called: ",Tagvars]),
    {_,Url} = proplists:lookup(url, Tagvars),
    {_,Bapp} = proplists:lookup(app, Tagvars),
    App = erlang:binary_to_atom(Bapp, utf8),
    Etsrecord = lookup_cache_url(App,Url),
    erlang:list_to_binary([?ASSETDIR,<<"/">>,Url,<<"?">>,Etsrecord#cachefile.md5]).

%% private functions
lookup_cache_url(App,Url) ->
    case ets:lookup(erlyassets_ets_table,Url) of
        [] -> 
            create_cache_url(App,Url);
        [Etsrecord] -> 
            update_cache_url_maybe(Etsrecord)
    end.

create_cache_url(App,Url) ->
    ?DPRINT(["create_cache_url called: ",Url]),
    Assetfile = filename:join([code:priv_dir(App), Url]),
    Lastmod = filelib:last_modified(Assetfile),
    {ok, Cachedata} = file:read_file(Assetfile),
    Md5file = md5(Cachedata),
    Etsrecord = #cachefile{app=App,
                           url=Url,
                           assetfile=Assetfile,
                           lastmod=Lastmod,
                           lastmodrfc1123 = cowboy_clock:rfc1123(Lastmod),
                           cachedata=Cachedata,
                           md5=Md5file,
                           lastused=erlang:localtime()
                           },
    ets:insert(erlyassets_ets_table,Etsrecord),
    Etsrecord.

update_cache_url_maybe(#cachefile{app=App,url=Url,lastmod=Lastmod} = Etsrecord) ->
    Assetfile = filename:join([code:priv_dir(App), Url]),
    Currentlastmod = filelib:last_modified(Assetfile),
    case Currentlastmod > Lastmod of
        true ->
            create_cache_url(App,Url);
        _ ->
            update_last_used(Etsrecord)
    end.

update_last_used(#cachefile{url=Url} = Etsrecord) ->
    Index = #cachefile.lastused,
    Curtime = erlang:localtime(),
    ets:update_element(erlyassets_ets_table,Url,{Index,Curtime}),
    Etsrecord#cachefile{lastused=Curtime}.

md5(S) ->
    erlang:list_to_binary(string:to_upper(lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= erlang:md5(S)]))).

