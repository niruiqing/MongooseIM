%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2016 17:55
%%%-------------------------------------------------------------------
-module(mongoose_api_auth).
-author("ludwikbukowski").
-include("mongoose_api.hrl").
-include("jlib.hrl").
-include("ejabberd.hrl").

%% ejabberd_cowboy exports
-export([cowboy_router_paths/2, to_json/2, from_json/2]).

%% API
-export([is_authorized/2,
         init/3, rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         rest_terminate/2,
         delete_resource/2]).

-import(mongoose_api_utils, [action_to_method/1, method_to_action/1, error_code/1]).

%%--------------------------------------------------------------------
%% ejabberd_cowboy callbacks
%%--------------------------------------------------------------------
%% @doc This is implementation of ejabberd_cowboy callback. Returns list of all available http paths.
-spec cowboy_router_paths(ejabberd_cowboy:path(), ejabberd_cowboy:options()) ->
    ejabberd_cowboy:implemented_result() | ejabberd_cowboy:default_result().
cowboy_router_paths(Base, _Opts) ->
    ejabberd_hooks:add(register_command, global, mongoose_api_utils, reload_dispatches, 50),
    ejabberd_hooks:add(unregister_command, global, mongoose_api_utils, reload_dispatches, 50),
    try
        Commands = ?COMMANDS_ENGINE:list(user),
        [handler_path(Base, Command) || Command <- Commands]
    catch
        _:Err ->
            ?ERROR_MSG("Error occured when getting the commands list: ~p~n", [Err]),
            []
    end.


%%--------------------------------------------------------------------
%% cowboy_rest callbacks
%%--------------------------------------------------------------------

init({_Transport, _}, Req, Opts) ->
    {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, Opts) ->
    {Bindings, Req1} = cowboy_req:bindings(Req),
    CommandCategory =
        case lists:keytake(command_category, 1, Opts) of
            {value, {command_category, Name},  _Opts1} ->
                Name;
            false ->
                undefined
        end,
    State = #backend_state{allowed_methods = mongoose_api_utils:get_allowed_methods(user),
        bindings = Bindings, command_category = CommandCategory},
    {ok, Req1, State}.

allowed_methods(Req, #backend_state{command_category = Name} = State) ->
    CommandList = ?COMMANDS_ENGINE:list(user, Name),
    AllowedMethods = [action_to_method(?COMMANDS_ENGINE:action(Command)) || Command <- CommandList],
    {AllowedMethods, Req, State}.

content_types_provided(Req, State) ->
    CTP = [{{<<"application">>, <<"json">>, '*'}, to_json}],
    {CTP, Req, State}.

content_types_accepted(Req, State) ->
    CTA = [{{<<"application">>, <<"json">>, '*'}, from_json}],
    {CTA, Req, State}.

rest_terminate(_Req, _State) ->
    ok.

is_authorized(Req, State) ->
    Auth = cowboy_req:parse_header(<<"authorization">>, Req),
    case Auth of
        {ok, undefined, _} ->
            make_unauthorized_response(Req, State);
        {ok, AuthDetails, Req2} ->
            do_authorize(AuthDetails, Req2, State)
    end.

%% @doc Called for a method of type "DELETE"
delete_resource(Req, #backend_state{command_category = Category} = State) ->
    [Command] = ?COMMANDS_ENGINE:list(user, Category, method_to_action(<<"DELETE">>)),
    mongoose_api_utils:process_request(<<"DELETE">>, Command, Req, State).

%%--------------------------------------------------------------------
%% internal callbacks
%%--------------------------------------------------------------------

%% @doc Called for a method of type "GET"
to_json(Req, #backend_state{command_category = Category} = State) ->
    [Command] = ?COMMANDS_ENGINE:list(user, Category, method_to_action(<<"GET">>)),
    mongoose_api_utils:process_request(<<"GET">>, Command, Req, State).


%% @doc Called for a method of type "POST" and "PUT"
from_json(Req, #backend_state{command_category = Category} = State) ->
    {Method, Req2} = cowboy_req:method(Req),
    [Command] = ?COMMANDS_ENGINE:list(user, Category, method_to_action(Method)),
    mongoose_api_utils:process_request(Method, Command, Req2, State).


do_authorize({<<"basic">>, {User, Password}}, Req, State) ->
    case jid:from_binary(User) of
        error ->
            make_unauthorized_response(Req, State);
        JID ->
            do_check_password(JID, Password, Req, State)
    end;
do_authorize(_, Req, State) ->
    make_unauthorized_response(Req, State).

do_check_password(#jid{luser = User, lserver = Server} = JID,
    Password, Req, State) ->
    case ejabberd_auth:check_password(User, Server, Password) of
        true ->
            {true, Req, State#backend_state{entity = jid:to_binary(JID)}};
        _ ->
            make_unauthorized_response(Req, State)
    end.

make_unauthorized_response(Req, State) ->
    {{false, <<"Basic realm=\"mongooseim\"">>}, Req, State}.

-spec handler_path(ejabberd_cowboy:path(), mongoose_command()) -> ejabberd_cowboy:path().
handler_path(Base, Command) ->
    {[Base, mongoose_api_utils:create_user_url_path(Command)],
        ?MODULE, [{command_category, ?COMMANDS_ENGINE:category(Command)}]}.