%%%-------------------------------------------------------------------
%% @doc dapper public API
%% @end
%%%-------------------------------------------------------------------

-module(dapper_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [{"/", dapper_serve, []}]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    dapper_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http),
    ok.

%% internal functions
