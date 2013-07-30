
-record(cachefile, {url,app,assetfile,lastmod,lastmodrfc1123,cachedata,md5,lastused}).

% -define(DPRINT(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-define(DPRINT(X), true).

-define(ASSETDIR, "/assets").

