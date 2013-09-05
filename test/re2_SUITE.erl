-module(re2_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


suite() ->
    [{timetrap, {seconds, 30}}].


all() ->
    %% [full_match_basic].
    [full_match_basic_true,
     full_match_basic_false,
     full_match_badarg,
     prepare,
     full_match_prepared_true,
     full_match_prepared_false,
     clear_prepared].


init_per_suite(Config) ->
    ok = application:start(re2),
    Config.


end_per_suite(_Config) ->
    ok = application:stop(re2),
    ok.


init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    re2:clear_prepared(),
    ok.


full_match_basic_true(_Config) ->
    ?assertEqual(true, re2:full_match("Hello", "(?i)h.*o")).


full_match_basic_false(_Config) ->
    ?assertEqual(false, re2:full_match("Hello", "h.*o")).


full_match_badarg(_Config) ->
    Result = (catch re2:full_match(1, 2)),
    ?assertMatch({'EXIT', {badarg, _}}, Result).


prepare(_Config) ->
    re2:prepare("h.*o", hello),
    ?assertEqual(1, re2:get_nr_prepared()),

    re2:remove_prepared(hello),
    ?assertEqual(0, re2:get_nr_prepared()).


full_match_prepared_true(_Config) ->
    re2:prepare("h.*o", hello_pattern),
    ?assertEqual(true, re2:full_match("hello", hello_pattern)).


full_match_prepared_false(_Config) ->
    re2:prepare("h.*o", hello_pattern),
    ?assertEqual(false, re2:full_match("hella", hello_pattern)).


clear_prepared(_Config) ->
    ok = re2:prepare("h.*o", hello_pattern),
    re2:clear_prepared(),
    ?assertEqual(0, re2:get_nr_prepared()).
