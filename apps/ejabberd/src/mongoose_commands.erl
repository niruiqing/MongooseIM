%% @headerfile "mongoose_commands.hrl"
%%
%% @doc Mongoose version of command management
%% The following is loosely based on old ejabberd_commands implementation,
%% with some modification related to type check, permission control
%% and the likes.
%%
%% This is a central registry of commands which can be exposed via
%% REST, XMPP as ad-hoc commands or in any other way. Any module can
%% define its commands and register them here.
%%
%% ==== Usage ====
%%
%% A module defines a list of commands; a command definition is a proplist
%% with the following keys:
%%      name :: atom()
%% name of the command by which we refer to it
%%      category :: [atom()]
%% this defines what group the command belongs to, like user, chatroom etc
%%      desc :: string()
%% long description
%%      module :: module()
%% module to call
%%      function :: atom()
%% function to call
%%      action :: command_action()
%% so that the HTTP side can decide which verb to require
%%      args = [] :: [argspec()]
%% Type spec - see below; this is both for introspection and type check on call. Args spec is more limited
%% then return, it has to be a list of named arguments, like [{id, integer}, {msg, binary}]
%%      security_policy = [atom()] (optional)
%% permissions required to run this command, defaults to [admin]
%%      result :: argspec()
%% Type spec of return value of the function to call; execute/3 eventually returns {ok, result}
%%      identifiers :: [atom()] (optional, required in 'update' commands)
%%
%% You can ignore return value of the target func by specifying return value as {result, ok}. The
%% execute/3 will then always return just 'ok' (or error).
%%
%% If action is 'update' then it MUST specify which args are to be used as identifiers of object to update.
%% It has no effect on how the engine does its job, but may be used by client code to enforce proper
%% structure of request. (this is bad programming practice but we didn't have a better idea, we had to
%% solve it for REST API)
%%
%% Commands are registered here upon the module's initialisation
%% (the module has to explicitly call mongoose_commands:register_commands/1
%% func, it doesn't happen automagically), also should be unregistered when module
%% terminates.
%%
%% Commands are executed by calling mongoose_commands:execute/3 method. This
%% can return:
%% {ok, Result}
%% {error, denied, Msg} if user has no permission
%% {error, not_implemented, Msg}
%% {error, type_error, Msg} if either arguments or return value does not match
%% {error, internal, Msg} if an exception was caught
%%
%% ==== Type check ====
%%
%% A command's definition includes specification of it arguments; when
%% it is called, arguments are check for compatibility. Examples of specs
%% and compliant arguments:
%%
%% a single type spec
%% integer                          2
%% binary                           <<"zzz">>
%% atom                             brrr
%% a list of arbitrary length, of a given type
%% [integer]                        []
%% [integer]                        [1]
%% [integer]                        [1,2,3,4]
%% a list of anything
%% []
%% a named argument (name is only for clarity)
%% {msg, binary}                    <<"zzz">>
%% a tuple of args
%% {integer, binary, float}         {1, <<"2">>, 3.0}
%% a tuple of named args
%% {{x, integer}, {y, binary}}      {1, <<"bbb">>}
%%
%% Arg specification is used at call-time for control, and also for introspection
%% (see list/1, list/2, mongoose_commands:get_command/2 and args/1)
%%
%% Return value is also specified, and this is a bit tricky: command definition
%% contains spec of return value - what the target func returns should comply to it.
%% The registry, namely execute/3, returns a tuple {ok, ValueReturnedByTheFunction}
%% If return value is defined as 'ok' then whatever target func returns is ignored.
%% This is mostly to make a distinction between 'create' actions which actually create something
%% and return its identifier and those 'lame creators' which cause some action to be done and
%% something written to dbase (exemplum: sending a message), but there is no accessible resource.
%%
%% Called function may also return a tuple {error, term()}, this is returned by the registry
%% as {error, internal, Msg::binary()}
%%
%% ==== Permission control ====
%%
%% First argument to every function exported from this module is always
%% a user. If you call it from trusted place, you can pass 'admin' here and
%% the whole permission check is skipped. Otherwise, pass #jid record.
%%
%% A command MAY define a security policy to be applied
%% (and this is not yet designed)
%% If it doesn't, then the command is accessible to 'admin' calls only.
%%

-module(mongoose_commands).
-author("bartlomiej.gorny@erlang-solutions.com").
-include("mongoose_commands.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").

-type mongoose_command() :: #mongoose_command{}.
-type caller() :: admin|binary().

%%%% API

-export([check_type/2]).
-export([init/0]).

-export([register/1,
         unregister/1,
         list/1,
         list/2,
         list/3,
         register_commands/1,
         unregister_commands/1,
         get_command/2,
         execute/3,
         name/1,
         category/1,
         desc/1,
         args/1,
         identifiers/1,
         action/1,
         result/1
    ]).


%% @doc Register mongoose commands. This can be run by any module that wants its commands exposed.
-spec register([{atom(), term()}]) -> ok.
register(Cmds) ->
    init(),
    Commands = [check_command(C) || C <- Cmds],
    register_commands(Commands).

%% @doc Unregister mongoose commands. Should be run when module is unloaded.
-spec unregister([{atom(), term()}]) -> ok.
unregister(Cmds) ->
    Commands = [check_command(C) || C <- Cmds],
    unregister_commands(Commands).

%% @doc List commands, available for this user.
-spec list(caller()) -> [mongoose_command()].
list(U) ->
    list(U, any, any).

%% @doc List commands, available for this user, filtered by category.
-spec list(caller(), atom()) -> [mongoose_command()].
list(U, C) ->
    list(U, C, any).

%% @doc List commands, available for this user, filtered by category and action.
-spec list(caller(), atom(), atom()) -> [mongoose_command()].
list(U, Category, Action) ->
    CL = command_list(Category, Action),
    lists:filter(fun(C) -> is_available_for(U, C) end, CL).

%% @doc Get command definition, if allowed for this user.
-spec get_command(caller(), atom()) -> mongoose_command().
get_command(Caller, Name) ->
    case ets:lookup(mongoose_commands, Name) of
        [C] ->
            case is_available_for(Caller, C) of
                true ->
                    C;
                false ->
                    {error, denied, <<"Command not available">>}
            end;
        [] -> {error, not_implemented, <<"Command not implemented">>}
    end.

%% accessors
-spec name(mongoose_command()) -> atom().
name(Cmd) ->
    Cmd#mongoose_command.name.

-spec category(mongoose_command()) -> atom().
category(Cmd) ->
    Cmd#mongoose_command.category.

-spec desc(mongoose_command()) -> list().
desc(Cmd) ->
    Cmd#mongoose_command.desc.

-spec args(mongoose_command()) -> term().
args(Cmd) ->
    Cmd#mongoose_command.args.

-spec identifiers(mongoose_command()) -> [atom()].
identifiers(Cmd) ->
    Cmd#mongoose_command.identifiers.

-spec action(mongoose_command()) -> command_action().
action(Cmd) ->
    Cmd#mongoose_command.action.

-spec result(mongoose_command()) -> term().
result(Cmd) ->
    Cmd#mongoose_command.result.

%% @doc Command execution.
-spec execute(caller(), atom()|mongoose_command(), [term()]|map()) -> {ok, term()} | ok | failure().
execute(Caller, Name, Args) when is_atom(Name) ->
    case ets:lookup(mongoose_commands, Name) of
        [Command] -> execute_command(Caller, Command, Args);
        [] -> {error, not_implemented, <<"This command is not supported">>}
    end;
execute(Caller, #mongoose_command{name = Name}, Args) ->
    execute(Caller, Name, Args).

init() ->
    case ets:info(mongoose_commands) of
        undefined ->
            ets:new(mongoose_commands, [named_table, set, public,
                {keypos, #mongoose_command.name}]);
        _ ->
            ok
    end.

%%%% end of API

-spec register_commands([mongoose_command()]) -> ok.
register_commands(Commands) ->
    lists:foreach(
        fun(Command) ->
            check_registration(Command), %% may throw
            ets:insert_new(mongoose_commands, Command),
            ejabberd_hooks:run(register_command, [Command]),
            ok
        end,
        Commands).


-spec unregister_commands([mongoose_command()]) -> ok.
unregister_commands(Commands) ->
    lists:foreach(
        fun(Command) ->
            ets:delete_object(mongoose_commands, Command),
            ejabberd_hooks:run(unregister_command, [Command])
        end,
        Commands).

execute_command(Caller, Command, Args) ->
    try check_and_execute(Caller, Command, Args) of
        ignore_result ->
            ok;
        Res ->
            {ok, Res}
    catch
        {type_mismatch, E} ->
            {error, type_error, E};
        permission_denied ->
            {error, denied, <<"Command not available for this user">>};
        caller_jid_mismatch ->
            {error, denied, <<"Caller ids do not match">>};
        X:E ->
            ?ERROR_MSG("Caught ~p:~p while executing ~p", [X, E, Command#mongoose_command.name]),
            {error, internal, term_to_binary(E)}
    end.

%% @doc This performs many checks - types, permissions etc, may throw one of many exceptions
%% returns what the func returned or just ok if command spec tells so
-spec check_and_execute(caller(), mongoose_command(), [term()]) -> term().
check_and_execute(Caller, Command, Args) when is_map(Args) ->
    check_and_execute(Caller, Command, map_to_list(Args, Command#mongoose_command.args));
check_and_execute(Caller, Command, Args) ->
    % check permissions
    case is_available_for(Caller, Command) of
        true ->
            ok;
        false ->
            throw(permission_denied)
    end,
    % check caller (if it is given in args, and the engine is called by a 'real' user, then it must match
    check_caller(Caller, Command, Args),
    % check args
    SpecLen = length(Command#mongoose_command.args),
    ALen = length(Args),
    if SpecLen =/= ALen ->
        th("Invalid number of arguments: should be ~p, got ~p", [SpecLen, ALen]);
        true -> ok
    end,
    [check_type(S, A) || {S, A} <- lists:zip(Command#mongoose_command.args, Args)],
    % run command
    Res = apply(Command#mongoose_command.module, Command#mongoose_command.function, Args),
    case Res of
        {error, E} ->
            throw({func_returned_error, E});
        _ ->
            % transitional
            ResSpec = case Command#mongoose_command.result of
                            {ok, R} -> R;
                            R -> R
                      end,
            check_type(ResSpec, Res),
            maybe_ignore_result(ResSpec, Res)
    end.

maybe_ignore_result(ok, _) ->
    ignore_result;
maybe_ignore_result(_, Res) ->
    Res.

check_type(ok, _) ->
    ok;
check_type(A, A) ->
    true;
check_type({_Name, binary}, Value) when is_binary(Value) ->
    true;
check_type({_Name, integer}, Value) when is_integer(Value) ->
    true;
check_type(Spec, Value) when is_tuple(Spec) and not is_tuple(Value) ->
    th("~p is not a tuple", [Value]);
check_type(Spec, Value) when is_tuple(Spec) ->
    compare_tuples(Spec, Value);
check_type([_Spec], []) ->
    true;
check_type([Spec], [H|T]) ->
    check_type({none, Spec}, H),
    check_type([Spec], T);
check_type([], [_|_]) ->
    true;
check_type([], []) ->
    true;
check_type(Spec, Value) ->
    th("Catch-all: ~p vs ~p", [Spec, Value]).


compare_tuples(Spec, Val) ->
    Ssize = tuple_size(Spec),
    Vsize = tuple_size(Val),
    case Ssize of
        Vsize ->
            compare_lists(tuple_to_list(Spec), tuple_to_list(Val));
        _ ->
            th("Tuples of different size: ~p and ~p", [Spec, Val])
    end.


compare_lists([], []) ->
    true;
compare_lists([S|Sp], [V|Val]) ->
    check_type(S, V),
    compare_lists(Sp, Val).


th(Fmt, V) ->
    throw({type_mismatch, io_lib:format(Fmt, V)}).


check_command(PL) ->
    Fields = record_info(fields, mongoose_command),
    Lst = check_command([], PL, Fields),
    RLst = lists:reverse(Lst),
    Cmd = list_to_tuple([mongoose_command|RLst]),
    check_identifiers(Cmd#mongoose_command.action, Cmd#mongoose_command.identifiers, Cmd#mongoose_command.args),
    % store position of caller in args (if present)
    Cmd#mongoose_command{caller_pos = locate_caller(Cmd#mongoose_command.args)}.

check_identifiers(update, [], _) ->
    baddef(identifiers, empty);
check_identifiers(update, Ids, Args) ->
    check_identifiers(Ids, Args);
check_identifiers(_, _, _) ->
    ok.

check_identifiers([], _) ->
    ok;
check_identifiers([H|T], Args) ->
    case proplists:get_value(H, Args) of
        undefined -> baddef(H, missing);
        _ -> check_identifiers(T, Args)
    end.

check_command(Cmd, _PL, []) ->
    Cmd;
check_command(Cmd, PL, [N|Tail]) ->
    V = proplists:get_value(N, PL),
    Val = check_value(N, V),
    check_command([Val|Cmd], PL, Tail).


check_value(name, V) when is_atom(V) ->
    V;
check_value(category, V) when is_atom(V) ->
    V;
check_value(desc, V) when is_list(V) ->
    V;
check_value(module, V) when is_atom(V) ->
    V;
check_value(function, V) when is_atom(V) ->
    V;
check_value(action, read) ->
    read;
check_value(action, send) ->
    send;
check_value(action, create) ->
    create;
check_value(action, update) ->
    update;
check_value(action, delete) ->
    delete;
check_value(args, V) when is_list(V) ->
    Filtered = [C || {C, _} <- V],
    if
        length(V) =/= length(Filtered) -> baddef(args, V);
        true -> V
    end;
check_value(security_policy, undefined) ->
    [admin];
check_value(security_policy, []) ->
    baddef(security_policy, empty);
check_value(security_policy, V) when is_list(V) ->
    lists:map(fun check_security_policy/1, V),
    V;
check_value(result, undefined) ->
    baddef(result, undefined);
check_value(result, V) ->
    V;
check_value(identifiers, undefined) ->
    [];
check_value(identifiers, V) ->
    V;
check_value(caller_pos, _) ->
    0;
check_value(K, V) ->
    baddef(K, V).


%% @doc Known security policies
check_security_policy(user) ->
    ok;
check_security_policy(admin) ->
    ok;
check_security_policy(Other) ->
    baddef(security_policy, Other).

baddef(K, V) ->
    throw({invalid_command_definition, io_lib:format("~p=~p", [K, V])}).

command_list(Category, Action) ->
    Cmds = [C || [C] <- ets:match(mongoose_commands, '$1')],
    filter_commands(Category, Action, Cmds).

filter_commands(any, any, Cmds) ->
    Cmds;
filter_commands(Cat, any, Cmds) ->
    [C || C <- Cmds, C#mongoose_command.category == Cat];
filter_commands(any, _, _) ->
    throw({invalid_filter, ""});
filter_commands(Cat, Action, Cmds) ->
    [C || C <- Cmds, C#mongoose_command.category == Cat, C#mongoose_command.action == Action].

%% @doc make sure the command may be registered
%% it may not if either (a) command of that name is already registered,
%% (b) there is a command in the same category with the same action
check_registration(Command) ->
    Name = name(Command),
    Cat = category(Command),
    Act = action(Command),
    case ets:lookup(mongoose_commands, Name) of
        [] ->
            CatLst = list(admin, Cat),
            FCatLst = [C || C <- CatLst, C#mongoose_command.action == Act],
            case FCatLst of
                [] -> ok;
                [C] ->
                    baddef("There is command ~p in category ~p, action ~p", [name(C), Cat, Act])
            end;
        _ ->
            ?DEBUG("This command is already defined:~n~p", [Name])
    end.

mapget(K, Map) ->
    try maps:get(K, Map) of
        V -> V
    catch
        error:{badkey, K} ->
            th("Missing argument: ~p", [K]);
        error:bad_key ->
            th("Missing argument: ~p", [K])
    end.

map_to_list(Map, Args) ->
    SpecLen = length(Args),
    ALen = maps:size(Map),
    if SpecLen =/= ALen ->
        th("Invalid number of arguments: should be ~p, got ~p", [SpecLen, ALen]);
        true -> ok
    end,
    [mapget(K, Map) || {K, _} <- Args].


%% @doc Main entry point for permission control - is this command available for this user
is_available_for(User, C) when is_binary(User) ->
    is_available_for(jid:from_binary(User), C);
is_available_for(admin, _C) ->
    true;
is_available_for(Jid, #mongoose_command{security_policy = Policies}) ->
    apply_policies(Policies, Jid).

%% @doc Check all security policies defined in the command - passes if any of them returns true
apply_policies([], _) ->
    false;
apply_policies([P|Policies], Jid) ->
    case apply_policy(P, Jid) of
        true ->
            true;
        false ->
            apply_policies(Policies, Jid)
    end.

%% @doc This is the only policy we know so far, but there will be others (like roles/acl control)
apply_policy(user, _) ->
    true;
apply_policy(_, _) ->
    false.


locate_caller(L) ->
    locate_caller(1, L).

locate_caller(I, []) ->
    0;
locate_caller(I, [{caller, _}|_]) ->
    I;
locate_caller(I, [_|T]) ->
    locate_caller(I + 1, T).


check_caller(admin, _Command, _Args) ->
    ok;
check_caller(_Caller, #mongoose_command{caller_pos = 0}, _Args) ->
    % no caller in args
    ok;
check_caller(Caller, #mongoose_command{caller_pos = CallerPos, name = Name}, Args) ->
    % check that server and user match (we don't care about resource)
    ACaller = lists:nth(CallerPos, Args),
    CallerJid = jid:from_binary(Caller),
    ACallerJid = jid:from_binary(ACaller),
    ACal = {ACallerJid#jid.user, ACallerJid#jid.server},
    case {CallerJid#jid.user, CallerJid#jid.server} of
        ACal ->
            ok;
        _ ->
            throw(caller_jid_mismatch)
    end.











