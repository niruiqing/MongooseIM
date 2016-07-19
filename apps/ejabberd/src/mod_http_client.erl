-module(mod_http_client).
-behaviour(gen_mod).
-behaviour(gen_server).

-export([start_link/1, start/2, stop/1, start_pool/3, get_pool/2]).

-export([get_path_prefix/1, make_request/7]).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-define(DEFAULT_HTTP_POOL_SIZE, 5).
-define(DEFAULT_HTTP_HOST, "http://localhost").
-define(DEFAULT_PREFIX_PATH, <<"/">>).
-define(DEFAULT_HTTP_WORKER_TIMEOUT, 5000).
-define(DEFAULT_HTTP_POOL_TIMEOUT, 200).

-record(http_pool, {name, host, http_host, size, path_prefix}).
-record(state, {pools}).

-opaque http_pool() :: #http_pool{}.

-export_type([http_pool/0]).

%% gen_server callbacks

init(_) ->
    {ok, #state{pools = dict:new()}}.

handle_call({start_pool, Host, Name, Opts}, _From, State = #state{pools = Pools}) ->
    Pool = mk_pool(Host, Name, Opts),
    do_start_pool(Pool),
    {reply, ok, State#state{pools = dict:store(Name, Pool, Pools)}};
handle_call({get_pool, Name}, _From, State = #state{pools = Pools}) ->
    {reply, dict:fetch(Name, Pools), State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%

start(Host, Opts) ->
    {ok, SupProc} = start_supervisor(Host),
    Proc = gen_mod:get_module_proc(Host, mod_http_client),
    ChildSpec = {Proc,
                 {?MODULE, start_link, [Host]},
                 permanent,
                 1000,
                 worker,
                 [?MODULE]},
    {ok, _} = supervisor:start_child(SupProc, ChildSpec),
    PoolSpec = gen_mod:get_opt(pools, Opts, []),
    [start_pool(Host, Name, PoolOpts) || {Name, PoolOpts} <- PoolSpec].

start_pool(Host, Name, Opts) ->
    Proc = gen_mod:get_module_proc(Host, mod_http_client),
    gen_server:call(Proc, {start_pool, Host, Name, Opts}).

get_pool(Host, Name) ->
    Proc = gen_mod:get_module_proc(Host, mod_http_client),
    gen_server:call(Proc, {get_pool, Name}).

start_link(Host) ->
    Proc = gen_mod:get_module_proc(Host, mod_http_client),
    gen_server:start_link({local, Proc}, ?MODULE, Host, []).

stop(Host) ->
    stop_supervisor(Host).

mk_pool(Host, Name, Opts) ->
    HttpHost = gen_mod:get_opt(host, Opts, ?DEFAULT_HTTP_HOST),
    PoolSize = gen_mod:get_opt(pool_size, Opts, ?DEFAULT_HTTP_POOL_SIZE),
    PathPrefix = gen_mod:get_opt(path_prefix, Opts, ""),
    #http_pool{name = Name,
               host = Host,
               http_host = HttpHost,
               size = PoolSize,
               path_prefix = list_to_binary(PathPrefix)}.

get_path_prefix(#http_pool{path_prefix = PathPrefix}) -> PathPrefix.

do_start_pool(#http_pool{name = Name,
                         host = Host,
                         http_host = HttpHost,
                         size = PoolSize}) ->
    ProcName = pool_proc_name(Host, Name),
    PoolOpts = [{name, {local, ProcName}},
                {size, PoolSize},
                {max_overflow, 5},
                {worker_module, mod_http_client_worker}],
    SupProc = gen_mod:get_module_proc(Host, ejabberd_mod_http_client_sup),
    {ok, _} = supervisor:start_child(
                SupProc,
                poolboy:child_spec(ProcName, PoolOpts, [HttpHost, []])),
    ok.

pool_proc_name(Host, PoolName) ->
    gen_mod:get_module_proc(Host, PoolName).

%% stop_pool(Host, PoolName) ->
%%     SupProc = gen_mod:get_module_proc(Host, ejabberd_mod_http_client_sup),
%%     supervisor:terminate_child(SupProc, PoolName),
%%     supervisor:delete_child(SupProc, PoolName),
%%     ok.

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
                 pool_proc_name(Host, PoolName),
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
