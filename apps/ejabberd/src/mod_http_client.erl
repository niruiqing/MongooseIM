-module(mod_http_client).
-behaviour(gen_mod).

-export([start/2, stop/1]).

-export([lookup_pool/2, get_path_prefix/1, make_request/7]).

-define(DEFAULT_HTTP_POOL_SIZE, 5).
-define(DEFAULT_HTTP_HOST, "http://localhost").
-define(DEFAULT_PREFIX_PATH, <<"/">>).
-define(DEFAULT_HTTP_WORKER_TIMEOUT, 5000).
-define(DEFAULT_HTTP_POOL_TIMEOUT, 200).

-record(http_pool, {id, http_host, size, path_prefix}).

-opaque http_pool() :: #http_pool{}.

-export_type([http_pool/0]).

start(Host, Opts) ->
    start_supervisor(Host),
    ets:new(mod_http_client_pools, [named_table, public, {keypos, 2}]),
    Pools = [mk_pool(Host, Name, PoolOpts) ||
                {Name, PoolOpts} <- gen_mod:get_opt(pools, Opts, [])],
    ets:insert(mod_http_client_pools, pools),
    lists:foreach(fun start_pool/1, Pools).

stop(Host) ->
    stop_supervisor(Host),
    ets:delete(mod_http_client_pools).

mk_pool(Host, Name, Opts) ->
    HttpHost = gen_mod:get_opt(host, Opts, ?DEFAULT_HTTP_HOST),
    PoolSize = gen_mod:get_opt(pool_size, Opts, ?DEFAULT_HTTP_POOL_SIZE),
    PathPrefix = gen_mod:get_opt(pool_size, Opts, ""),
    #http_pool{id = {Host, Name},
               http_host = HttpHost,
               size = PoolSize,
               path_prefix = PathPrefix}.

lookup_pool(Host, Name) ->
    [PoolSpec] = ets:lookup(mod_http_client_pools, {Host, Name}),
    PoolSpec.

get_path_prefix(#http_pool{path_prefix = PathPrefix}) -> PathPrefix.

start_pool(#http_pool{id = {Host, PoolName},
                      http_host = HttpHost,
                      size = PoolSize}) ->
    PoolOpts = [{name, {local, PoolName}},
                {size, PoolSize},
                {max_overflow, 5},
                {worker_module, mod_http_client_worker}],
    SupProc = gen_mod:get_module_proc(Host, ejabberd_mod_http_client_sup),
    {ok, _} = supervisor:start_child(
                SupProc,
                poolboy:child_spec(PoolName, PoolOpts, [HttpHost, []])),
    ok.

stop_pool(Host, PoolName) ->
    SupProc = gen_mod:get_module_proc(Host, ejabberd_mod_http_client_sup),
    supervisor:terminate_child(SupProc, PoolName),
    supervisor:delete_child(SupProc, PoolName),
    ok.

start_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host, ejabberd_mod_http_client_sup),
    ChildSpec =
        {Proc,
         {mod_http_client_sup, start_link, [Proc]},
         permanent,
         infinity,
         supervisor,
         [mod_http_client_sup]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host, ejabberd_mod_http_client_sup),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

make_request(Host, PoolName, Path, Method, Header, Query, Timeout) ->
    PoolTimeout = gen_mod:get_module_opt(Host, ?MODULE, pool_timeout, ?DEFAULT_HTTP_POOL_TIMEOUT),
    case catch poolboy:transaction(
                 PoolName,
                 fun(WorkerPid) ->
                         fusco:request(WorkerPid, Path, Method, Header, Query, Timeout)
                 end,
                 PoolTimeout) of
        {'EXIT', {timeout, _}} ->
            {error, poolbusy};
        {ok, {{Code, _Reason}, _RespHeaders, RespBody, _, _}} ->
            {ok, {Code, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.
