-module(re2_perf).

-compile(export_all).

test_data() ->
    Url_pattern =
        ("^(http|https|ftp)\://([a-zA-Z0-9\.\-]+(\:[a-zA-Z0-9\.&amp;%\$\-"
         "]+)*@)*((25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|"
         "[1-9])\.(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|"
         "[1-9]|0)\.(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|"
         "[1-9]{1}[0-9]{1}|[1-9]|0)\.(25[0-5]|2[0-4][0-9]|"
         "[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[0-9])|localhost|"
         "([a-zA-Z0-9\-]+\.)*[a-zA-Z0-9\-]+\.(com|edu|gov|int|mil|net|org"
         "|biz|arpa|info|name|pro|aero|coop|museum|[a-zA-Z]{2}))"
         "(\:[0-9]+)*(/($|[a-zA-Z0-9\.\,\?\'\\\+&amp;%\$#\=~_\-]+))*$"),

    [{{simple, true}, "hello", "^h.*o$"},
     {{simple, false}, "fail", "^h.*o$"},
     {{moderate, true}, "aaaccc", "^((a*)|(b*))((c*)|(d*))$"},
     {{moderate, false}, "abcd", "^((a*)|(b*))((c*)|(d*))$"},
     {{complex, true}, "http://www.example.com:80/path;arg=value?arg=value",
      Url_pattern},
     {{complex, false}, "http://www.example.com!:80/path;arg=value?arg=value",
      Url_pattern}].


tests() ->
    [{fun re/4, "re"},
     {fun re2/4, "re2"}].


re(Subj, Expression, Expected_result, Nr_times) ->
    {ok, Re} = re:compile(Expression),

    Result = re:run(Subj, Re) =/= nomatch,

    case Result of
        Expected_result ->
            Start = now(),
            lists:foreach(fun(_) -> re:run(Subj, Re, [{capture, none}]) end,
                          lists:seq(1, Nr_times)),
            End = now(),
            {ok, timer:now_diff(End, Start)};
        _ ->
            error
    end.


re2(Subj, Expression, Expected_result, Nr_times) ->
    re2:prepare(Expression, pattern),
    Result = re2:full_match(Subj, pattern),

    case Result of
        Expected_result ->
            Start = now(),
            lists:foreach(fun(_) -> re2:full_match(Subj, pattern) end,
                          lists:seq(1, Nr_times)),
            End = now(),
            {ok, timer:now_diff(End, Start)};
        _ ->
            error
    end.


run_tests() ->
    lists:foreach(fun run_test/1, test_data()).

run_test({Test={_Test_name, Expected_result}, Subj, Expression}) ->
    Nr_times = 10000,
    io:format("~nRunning test: ~p~n", [Test]),
    lists:foreach(
      fun({Callable, Description}) ->
              io:format("~s: ", [Description]),
              case Callable(list_to_binary(Subj), list_to_binary(Expression),
                            Expected_result, Nr_times) of
                  error ->
                      io:format("wrong answer~n");
                  {ok, Time} ->
                      io:format("~p~n", [Time])
              end
      end,
      tests()).
