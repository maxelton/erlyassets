Erlyassets
==========

Erlyassets is a very simple asset pipeline for erlydtl and cowboy.

Intro
-----

Erlyassets is a really simple asset pipeline that may not even be an asset pipeline.  

It doesn't support concatinating or minifying files, but it does automate the process of including a md5 hash of a static asset in the url and serving those assets with a far future expire date to minimize browser requests. Assets which are served with erlyassets are stored in an ets table and served from memory, so it is not suitable for serving large files.  

It consists of two parts:  

1. An erlydtl tag in erlyassets_dtl_tags called get_cache_url which you use in your templates to get a link to a static asset file.
2. An cowboy web server http handler called erlyassets_cowboy_handler which is used to serve said assets. 

See the examples folder for a simple example how to use this.  

If you are looking for a solution which can compile and minify assets also, consider using erlyassets along with my wbuildr tool [wbuildr](https://github.com/maxelton/wbuildr).  

Erlydtl part
------------

To use the get_cache_url function from erlyassets in your templates they have to be compiled with the {custom_tags_modules,[erlyassets_dtl_tags]} tag. To do this you will probably have to compile your templates in your own code and not rely on rebar or some other tool to do that for you.  

erlydtl:compile("templates/simple_assets.dtl", dtl_simple_assets, [{custom_tags_modules,[erlyassets_dtl_tags]}])  

That should make the get_cache_url function available in your template. The function call has the following signature.  

{% get_cache_url app="simple_assets" url="test.txt" %}  

It needs an application name and an url to the actual asset which must be located in the applications priv folder. If you need to troubleshoot this feature you should easily be able to see if your assets are being loaded by running the table viewer, tv:start(), and looking in the erlyassets_ets_table.  

Cowboy part
-----------

Once the template tag is working all you have to do is register a cowboy_http_handler like below:  

{?ASSETDIR ++ "/:asset", erlyassets_cowboy_handler, []}  

see the simple_assets_app.erl file for a full example. ?ASSETDIR is simply defined as /assets. This means you can not have a route with this name somewhere else in your app unless you change the define. You should not have an asset folder in your priv dir, this folder name is an abtraction added in the erlydtl template tag and removed again in this handler.  

That should be all there is to it!

Future work
-----------

It should be possible to support other web servers by adding handlers which function like the erlyassets_cowboy_handler.

Gotcha's
--------

 * Assets are stored in the ets table when the template is rendered not when the asset is requested. If you restart the webserver or clear the table between template rendering and requesting the asset you will not get a reply.
 * When you press F5 and observe web requests in a debugger you will often times see a 304 not modified response for static assets even though they have a far future expire date. This is only something the browser does to be extra safe after presseing F5 once or several times depending on the browser. To see that the asset cache is actually working, open a new tab and navigate to your page with the assets already in your cache, you should observe that requests for these assets are not being made.
 * Assets are currently not cleared from the ets table, so if you rename your assets frequently and you never restart this application or clear the table you will slowly leak memory. If you upload new assets with the same names, they will overwrite the previous assets in the ets table even though the md5 hash is different and no memory is wasted.

