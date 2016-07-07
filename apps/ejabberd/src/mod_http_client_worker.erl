-module(mod_http_client_worker).

%% API
-export([start_link/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link([Host, Opts]) ->
    fusco:start_link(Host, Opts).
