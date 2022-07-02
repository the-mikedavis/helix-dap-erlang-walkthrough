%% @doc Serves all HTTP requests.

-module(dapper_serve).

-export([init/2]).

init(Req0, Opts) ->
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           <<"Hello from Dapper!">>,
                           Req0),
    {ok, Req, Opts}.
