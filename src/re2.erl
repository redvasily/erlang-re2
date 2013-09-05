-module(re2).

-export([foo/1,
         bar/1,
         match/2,
         full_match/2,
         prepare/2,
         remove_prepared/1,
         get_nr_prepared/0,
         clear_prepared/0]).

-on_load(init/0).


init() ->
    %% ct:log("~p:~p", [?MODULE, init]),
    Module_path = code:which(?MODULE),
    App_path = filename:dirname(filename:dirname(Module_path)),
    Lib_path = filename:join([App_path, "priv", "re2_nif"]),
    %% io:format("Loading ~p~n", [Lib_path]),
    %% ct:log("Loading ~p~n", [Lib_path]),
    ok = erlang:load_nif(Lib_path, 0).
    %% ct:log("Loaded").


foo(_X) ->
    exit(nif_library_not_loaded).


bar(_Y) ->
    exit(nif_library_not_loaded).


match(_A, _B) ->
    exit(nif_library_not_loaded).


-spec full_match(Subj :: iolist(), Pattern :: iolist()) ->
                        boolean().
full_match(_Subj, _Pattern) ->
    exit(nif_library_not_loaded).


-spec prepare(Subj :: iolist(), Key :: atom()) ->
                     ok | {error, Reason :: any()}.
prepare(_Pattern, _Key) ->
    exit(nif_library_not_loaded).


-spec remove_prepared(Key :: atom()) ->
                             ok | {error, not_found}.
remove_prepared(_Key) ->
    exit(nif_library_not_loaded).


-spec get_nr_prepared() ->
                             non_neg_integer().
get_nr_prepared() ->
    exit(nif_library_not_loaded).


-spec clear_prepared() -> ok.
clear_prepared() ->
    exit(nif_library_not_loaded).
