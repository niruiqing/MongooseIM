%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Author: Joseph Yiasemides <joseph.yiasemides@erlang-solutions.com>
%% Description: Test HTTP Administration API for Mult-user Chat (MUC)
%%==============================================================================

-module(muc_http_api_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, positive}].

groups() ->
    [{positive, [shuffle], success_response()}].

success_response() ->
    [create_room].


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

%% init_per_group(_GroupName, Config) ->
%%     escalus:create_users(Config, escalus:get_users([alice])).


%% end_per_group(_GroupName, Config) ->
%%     escalus:delete_users(Config, escalus:get_users([alice])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Ancillary
%%--------------------------------------------------------------------

create_room(_Config) ->
    Body = #{name => <<"wonderland">>},
    Path = <<"/muc">>,
    {{<<"201">>, _}, <<"">>} = rest_helper:post(Path, Body).
