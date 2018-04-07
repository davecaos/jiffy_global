-module(jiffy_global_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_put_delete_test() ->
    K = '$$test$$mochiglobal',
    jiffy_global:delete(K),
    ?assertEqual(
       bar,
       jiffy_global:get(K, bar)),
    try
        jiffy_global:put(K, baz),
        ?assertEqual(
           baz,
           jiffy_global:get(K, bar)),
        jiffy_global:put(K, wibble),
        ?assertEqual(
           wibble,
           jiffy_global:get(K))
    after
        jiffy_global:delete(K)
    end,
    ?assertEqual(
       bar,
       jiffy_global:get(K, bar)),
    ?assertEqual(
       nil,
       jiffy_global:get(K)),
    ok.

-endif.
