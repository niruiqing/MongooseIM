%% @doc This suite tests both old ejabberd_commands module, which is slowly getting deprecated,
%% and the new mongoose_commands implementation.
-module(commands_backend_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/ejabberd_commands.hrl").
-include_lib("ejabberd/include/mongoose_commands.hrl").

-define(PORT, 5288).
-define(HOST, "localhost").
-define(IP,  {127,0,0,1}).

%% Error messages
-define(ARGS_LEN_ERROR, <<"Bad parameters length.">>).
-define(ARGS_SPEC_ERROR, <<"Bad name of the parameter.">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% suite configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

client_module() ->
    mongoose_api_auth.

backend_module() ->
    mongoose_api_backend.

all() ->
    [
        {group, simple},
        {group, get_advanced},
        {group, post_advanced},
        {group, delete_advanced},
        {group, client_simple}
    ].

groups() ->
    [
        {simple, [sequence],
            [
                get_simple,
                post_simple,
                delete_simple,
                put_simple
            ]
        },
        {get_advanced, [sequence],
            [
                get_two_args,
                get_wrong_path,
                get_wrong_arg_number,
                get_no_command,
                get_wrong_arg_type
            ]
        },
        {post_advanced, [sequence],
            [
                post_different_arg_order,
                post_wrong_arg_number,
                post_wrong_arg_name,
                post_wrong_arg_type,
                post_no_command
            ]
        },
        {delete_advanced, [sequence],
            [
                delete_wrong_arg_order,
                delete_wrong_arg_types
            ]
        },
        {put_advanced, [sequence],
            [
                put_wrong_type,
                put_wrong_param_type,
                put_wrong_bind_type,
                put_different_params_order,
                put_wrong_binds_order,
                put_too_less_params,
                put_too_less_binds,
                put_wrong_bind_name,
                put_wrong_param_name
            ]
        },
        {client_simple, [sequence],
            [
                get_simple_client,
                get_simple_client2]
        }
    ].

setup(Module) ->
    meck:unload(),
    meck:new(supervisor, [unstick, passthrough, no_link]),
    meck:new(ejabberd_hooks, []),
    meck:new(ejabberd_auth, []),
    %% you have to meck some stuff to get it working....
    meck:expect(ejabberd_auth, check_password, fun(_, _, _) -> true end),
    meck:expect(ejabberd_hooks, add, fun(_, _, _, _, _) -> ok end),
    meck:expect(ejabberd_hooks, run, fun(_, _) -> ok end),
    spawn(fun mc_holder/0),
    meck:expect(supervisor, start_child,
        fun(ejabberd_listeners, {_, {_, start_link, [_]}, transient,
            infinity, worker, [_]}) -> {ok, self()};
            (A,B) -> meck:passthrough([A,B])
        end),
    %% HTTP API config
    Opts = [{num_acceptors, 10},
        {max_connections, 1024},
        {modules, [{"localhost", "/api", Module, []}]}],
    ejabberd_cowboy:start_listener({?PORT, ?IP, tcp}, Opts).



teardown() ->
    ejabberd_cowboy:stop(ejabberd_cowboy:handler({?PORT, ?IP, tcp})),
    mongoose_commands:unregister(commands_new()),
    meck:unload(ejabberd_auth),
    meck:unload(ejabberd_hooks),
    meck:unload(supervisor),
    ok.

init_per_suite(C) ->
    application:ensure_all_started(cowboy),
    application:ensure_all_started(stringprep),
    application:ensure_all_started(fusco),
    application:ensure_all_started(lager),
    ok = mnesia:start(),
    C.

end_per_suite(C) ->
    stopped = mnesia:stop(),
    application:stop(lager),
    application:stop(fusco),
    application:stop(stringprep),
    application:stop(cowboy),
    C.

init_per_group(_, C) ->
    C.

end_per_group(_, C) ->
    C.

init_per_testcase(_, C) ->
    C.

end_per_testcase(_, C) ->
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% test methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_simple(_Config) ->
    Arg = {arg1, <<"bob@localhost">>},
    Base = "/api/users",
    ExpectedBody = get_simple_command(element(2, Arg)),
    {ok, Response} = get_request_admin(create_path_with_binds(Base, [Arg])),
    check_status_code(Response, 200),
    check_response_body(Response, ExpectedBody).

get_simple_client(_Config) ->
    Arg = {arg1, <<"bob@localhost">>},
    Base = "/api/clients",
    Username = <<"username@localhost">>,
    Auth = {binary_to_list(Username), "secret"},
    ExpectedBody = get_simple_client_command(Username, element(2, Arg)),
    {ok, Response} = get_request_auth(create_path_with_binds(Base, [Arg]), Auth),
    check_status_code(Response, 200),
    check_response_body(Response, ExpectedBody).

get_simple_client2(_Config) ->
    Arg1 = {other, <<"bob@localhost">>},
    Arg2 = {limit, 10},
    Base = "/api/message",
    Username = <<"alice@localhost">>,
    Auth = {binary_to_list(Username), "secret"},
    ExpectedBody = get_simple_client2_command(Username, element(2, Arg1), element(2, Arg2)),
    {ok, Response} = get_request_auth(create_path_with_binds(Base, [Arg1, Arg2]), Auth),
    check_status_code(Response, 200),
    check_response_body(Response, ExpectedBody).

delete_simple(_Config) ->
    Arg1 = {arg1, <<"ala_ma_kota">>},
    Arg2 = {arg2, 2},
    Base = "/api/music",
    {ok, Response} = delete_request(create_path_with_binds(Base, [Arg1, Arg2])),
    check_status_code(Response, 200).

post_simple(_Config) ->
    Arg1 = {arg1, 10},
    Arg2 = {arg2, 2},
    Args = [Arg1, Arg2],
    Path = <<"/api/weather">>,
    Result = binary_to_list(post_simple_command(element(2, Arg1), element(2, Arg2))),
    {ok, Response} = post_request(Path, Args),
    check_status_code(Response, 201),
    check_location_header(Response, list_to_binary("http://" ++ ?HOST ++ ":" ++ integer_to_list(?PORT) ++"/api/weather/" ++ Result)).

put_simple(_Config) ->
    Binds = [{arg1, <<"username">>}, {arg2,<<"localhost">>}],
    Args = [{arg3, <<"newusername">>}],
    Base = "/api/users",
    {ok, Response} = put_request(create_path_with_binds(Base, Binds), Args),
    check_status_code(Response, 200).

get_two_args(_Config) ->
    Arg1 = {arg1, 1},
    Arg2 = {arg2, 2},
    Base = "/api/animals",
    ExpectedBody = get_two_args_command(element(2, Arg1), element(2, Arg2)),
    {ok, Response} = get_request_admin(create_path_with_binds(Base, [Arg1, Arg2])),
    check_status_code(Response, 200),
    check_response_body(Response, ExpectedBody).

get_two_args_different_types(_Config) ->
    Arg1 = {one, 1},
    Arg2 = {two, <<"mybin">>},
    Base = "/api/books",
    ExpectedBody = get_two_args2_command(element(2, Arg1), element(2, Arg2)),
    {ok, Response} = get_request_admin(create_path_with_binds(Base, [Arg1, Arg2])),
    check_status_code(Response, 200),
    check_response_body(Response, ExpectedBody).

get_wrong_path(_Config) ->
    Path = <<"/api/animals/1/2">>,
    {ok, Response} = get_request_admin(Path),
    check_status_code(Response, 404).

get_wrong_arg_number(_Config) ->
    Path = <<"/api/animals/arg1/1/arg2/2/arg3/3">>,
    {ok, Response} = get_request_admin(Path),
    check_status_code(Response, 404).

get_no_command(_Config) ->
    Path = <<"/api/unregistered_command/123123">>,
    {ok, Response} = get_request_admin(Path),
    check_status_code(Response, 404).

get_wrong_arg_type(_Config) ->
    Path = <<"/api/animals/arg1/1/arg2/wrong">>,
    {ok, Response} = get_request_admin(Path),
    check_status_code(Response, 400).

post_wrong_arg_number(_Config) ->
    Args = [{arg1, 10}, {arg2,2}, {arg3, 100}],
    Path = <<"/api/weather">>,
    {ok, Response} = post_request(Path, Args),
    check_status_code(Response, 400).

post_wrong_arg_name(_Config) ->
    Args = [{arg11, 10}, {arg2,2}],
    Path = <<"/api/weather">>,
    {ok, Response} = post_request(Path, Args),
    check_status_code(Response, 400).

post_wrong_arg_type(_Config) ->
    Args = [{arg1, 10}, {arg2,<<"weird binary">>}],
    Path = <<"/api/weather">>,
    {ok, Response} = post_request(Path, Args),
    check_status_code(Response, 400).

post_different_arg_order(_Config) ->
    Arg1 = {arg1, 10},
    Arg2 = {arg2, 2},
    Args = [Arg2, Arg1],
    Path = <<"/api/weather">>,
    Result = binary_to_list(post_simple_command(element(2, Arg1), element(2, Arg2))),
    {ok, Response} = post_request(Path, Args),
    check_status_code(Response, 201),
    check_location_header(Response,
        list_to_binary("http://" ++ ?HOST ++ ":" ++ integer_to_list(?PORT) ++"/api/weather/" ++ Result)).

post_no_command(_Config) ->
    Args = [{arg1, 10}, {arg2,2}],
    Path = <<"/api/weather/10">>,
    {ok, Response} = post_request(Path, Args),
    check_status_code(Response, 404).


delete_wrong_arg_order(_Config) ->
    Arg1 = {arg1, <<"ala_ma_kota">>},
    Arg2 = {arg2, 2},
    Base = "/api/music",
    {ok, Response} = delete_request(create_path_with_binds(Base, [Arg2, Arg1])),
    check_status_code(Response, 404).

delete_wrong_arg_types(_Config) ->
    Arg1 = {arg1, 2},
    Arg2 = {arg2, <<"ala_ma_kota">>},
    Base = "/api/music",
    {ok, Response} = delete_request(create_path_with_binds(Base, [Arg1, Arg2])),
    check_status_code(Response, 400).

put_wrong_param_type(_Config) ->
    Binds = [{username, <<"username">>}, {domain, <<"domain">>}],
    Parameters = [{age, <<"23">>}, {kids, 10}],
    Base = "/api/dragons",
    {ok, Response} = put_request(create_path_with_binds(Base, Binds), Parameters),
    check_status_code(Response, 400).

put_wrong_bind_type(_Config) ->
    Binds = [{username, <<"username">>}, {domain, 123}],
    Parameters = [{age, 23}, {kids, 10}],
    Base = "/api/dragons",
    {ok, Response} = put_request(create_path_with_binds(Base, Binds), Parameters),
    check_status_code(Response, 400).

put_different_params_order(_Config) ->
    Binds = [{username, <<"username">>}, {domain, <<"domain">>}],
    Parameters = [{kids, 2}, {age, 45}],
    Base = "/api/dragons",
    {ok, Response} = put_request(create_path_with_binds(Base, Binds), Parameters),
    check_status_code(Response, 200).

put_wrong_binds_order(_Config) ->
    Binds = [{domain, <<"domain">>}, {username, <<"username">>}],
    Parameters = [{kids, 2}, {age, 30}],
    Base = "/api/dragons",
    {ok, Response} = put_request(create_path_with_binds(Base, Binds), Parameters),
    check_status_code(Response, 400).

put_too_less_params(_Config) ->
    Binds = [{username, <<"username">>}, {domain, <<"domain">>}],
    Parameters = [{kids, 3}],
    Base = "/api/dragons",
    {ok, Response} = put_request(create_path_with_binds(Base, Binds), Parameters),
    check_status_code(Response, 400).

put_too_less_binds(_Config) ->
    Binds = [{username, <<"username">>}],
    Parameters = [{age, 20}, {kids, 3}],
    Base = "/api/dragons",
    {ok, Response} = put_request(create_path_with_binds(Base, Binds), Parameters),
    check_status_code(Response, 404).

put_wrong_bind_name(_Config) ->
    Binds = [{usersrejm, <<"username">>}, {domain, <<"localhost">>}],
    Parameters = [{age, 20}, {kids, 3}],
    Base = "/api/dragons",
    {ok, Response} = put_request(create_path_with_binds(Base, Binds), Parameters),
    check_status_code(Response, 404).

put_wrong_param_name(_Config) ->
    Binds = [{username, <<"username">>}, {domain, <<"localhost">>}],
    Parameters = [{age, 20}, {srids, 3}],
    Base = "/api/dragons",
    {ok, Response} = put_request(create_path_with_binds(Base, Binds), Parameters),
    check_status_code(Response, 404).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
commands_client() ->
    [
        [
            {name, get_simple_client},
            {category, clients},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, get_simple_client_command},
            {action, read},
            {identifiers, []},
            {security_policy, [user]},
            {args, [{caller, binary}, {arg1, binary}]},
            {result, {result, binary}}
        ],
        [
            {name, get_simple_client2},
            {category, message},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, get_simple_client2_command},
            {action, read},
            {identifiers, []},
            {security_policy, [user]},
            {args, [{caller, binary}, {other, binary}, {limit, integer}]},
            {result, {result, binary}}
        ]
    ].

commands_admin() ->
    [
        [
            {name, get_simple},
            {category, users},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, get_simple_command},
            {action, read},
            {identifiers, []},
            {args, [{arg1, binary}]},
            {result, {result, binary}}
        ],
        [
            {name, get_advanced},
            {category, animals},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, get_two_args_command},
            {action, read},
            {identifiers, []},
            {args, [{arg1, integer}, {arg2, integer}]},
            {result, {result, binary}}
        ],
        [
            {name, get_advanced2},
            {category, books},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, get_two_args2_command},
            {action, read},
            {identifiers, []},
            {args, [{one, integer}, {two, binary}]},
            {result, {result, integer}}
        ],
        [
            {name, post_simple},
            {category, weather},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, post_simple_command},
            {action, create},
            {identifiers, []},
            {args, [{arg1, integer}, {arg2, integer}]},
            {result, {result, binary}}
        ],
        [
            {name, delete_simple},
            {category, music},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, delete_simple_command},
            {action, delete},
            {identifiers, []},
            {args, [{arg1, binary}, {arg2, integer}]},
            {result, ok}
        ],
        [
            {name, put_simple},
            {category, users},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, put_simple_command},
            {action, update},
            {args, [{arg1, binary}, {arg2, binary}, {arg3, binary}]},
            {identifiers, [arg1, arg2]},
            {result, ok}
        ],
        [
            {name, put_advanced},
            {category, dragons},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, put_advanced_command},
            {action, update},
            {args, [{username, binary},
                    {domain, binary},
                    {age, integer},
                    {kids, integer}]},
            {identifiers, [username, domain]},
            {result, ok}
        ]
        ].

commands_new() ->
    commands_admin() ++ commands_client().

get_simple_command(<<"bob@localhost">>) ->
    <<"bob is OK">>.

get_simple_client_command(_Caller, _SomeBinary) ->
    <<"client bob is OK">>.

get_simple_client2_command(_Caller, _SomeBinary, _SomeInteger) ->
    <<"client2 bob is OK">>.

get_two_args_command(1, 2) ->
    <<"all is working">>.

get_two_args2_command(X, B) when is_integer(X) and is_binary(B) ->
    100.

post_simple_command(_X, 2) ->
    <<"new_resource">>.

delete_simple_command(Binary, 2) when is_binary(Binary) ->
    10.

put_simple_command(_Arg1, _Arg2, _Arg3) ->
    ok.

put_advanced_command(Arg1, Arg2, Arg3, Arg4) when is_binary(Arg1) and is_binary(Arg2)
                                             and is_integer(Arg3) and is_integer(Arg4) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accepted_headers() ->
    [{<<"Content-Type">>, <<"application/json">>}, {<<"Accept">>, <<"application/json">>}].

maybe_add_auth_header({User, Password}) ->
    Basic = list_to_binary("basic " ++ base64:encode_to_string(User ++ ":"++ Password)),
    [{<<"authorization">>, Basic}];
maybe_add_auth_header(admin) ->
    [].

create_path_with_binds(Base, ArgList) when is_list(ArgList) ->
    list_to_binary(lists:flatten(Base ++
    ["/" ++ to_list(ArgName) ++ "/" ++ to_list(ArgValue) || {ArgName, ArgValue} <- ArgList])).

to_list(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_list(Float) when is_float(Float) ->
    float_to_list(Float);
to_list(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_list(Other) ->
    Other.


-spec get_request_admin(binary()) -> any().
get_request_admin(Path) ->
    setup(backend_module()),
    {ok, Pid} = fusco:start_link("http://"++ ?HOST ++ ":" ++ integer_to_list(?PORT), []),
    R = fusco:request(Pid, Path, "GET", [], [], 5000),
    fusco:disconnect(Pid),
    teardown(),
    R.

-spec get_request_auth(binary(), {binary(), binary()}) -> any().
get_request_auth(Path, Auth) ->
    setup(client_module()),
    AuthHeader = maybe_add_auth_header(Auth),
    {ok, Pid} = fusco:start_link("http://"++ ?HOST ++ ":" ++ integer_to_list(?PORT), []),
    R = fusco:request(Pid, Path, "GET", AuthHeader, [], 5000),
    fusco:disconnect(Pid),
    teardown(),
    R.

-spec post_request(binary(), [{atom(), any()}]) -> any().
post_request(Path, Args) ->
    setup(backend_module()),
    Body = jiffy:encode(maps:from_list(Args)),
    {ok, Pid} = fusco:start_link("http://"++ ?HOST ++ ":" ++ integer_to_list(?PORT), []),
    R = fusco:request(Pid, Path, "POST", accepted_headers(), Body, 5000),
    fusco:disconnect(Pid),
    teardown(),
    R.

-spec put_request(binary(), [{atom(), any()}]) -> any().
put_request(Path, Args) ->
    setup(backend_module()),
    Body = jiffy:encode(maps:from_list(Args)),
    {ok, Pid} = fusco:start_link("http://"++ ?HOST ++ ":" ++ integer_to_list(?PORT), []),
    R = fusco:request(Pid, Path, "PUT", accepted_headers(), Body, 5000),
    fusco:disconnect(Pid),
    teardown(),
    R.

-spec delete_request(binary()) -> any().
delete_request(Path) ->
    setup(backend_module()),
    {ok, Pid} = fusco:start_link("http://"++ ?HOST ++ ":" ++ integer_to_list(?PORT), []),
    R = fusco:request(Pid, Path, "DELETE", [], [], 5000),
    fusco:disconnect(Pid),
    teardown(),
    R.

mc_holder() ->
    mongoose_commands:init(),
    mongoose_commands:register(commands_new()),
    receive
        _ -> ok
    end.

check_status_code(Response, Code) when is_integer(Code) ->
    {{ResCode, _}, _, _, _, _} = Response,
    ?assertEqual(Code, binary_to_integer(ResCode));
check_status_code(_R, Code) ->
    ?assertEqual(Code, not_a_number).

check_response_body(Response, ExpectedBody) ->
    {_, _, Body, _ , _} = Response,
    ?assertEqual(binary_to_list(Body), "\"" ++ binary_to_list(ExpectedBody) ++ "\"").

check_location_header(Response, Path) ->
    {_, Headers, _, _ , _} = Response,
    Location = proplists:get_value(<<"location">>, Headers),
    ?assertEqual(Path, Location).
