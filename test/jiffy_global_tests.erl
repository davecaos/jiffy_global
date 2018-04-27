-module(jiffy_global_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_put_delete_test() ->
    K = '$$test$$mochiglobal',
    jiffy_global:delete(K),
    ?assertEqual(
       bar,
       jiffy_global:get_string_key(K, bar)),
    try
        jiffy_global:put_string_key(K, baz),
        ?assertEqual(
           baz,
           jiffy_global:get_string_key(K, bar)),
        jiffy_global:put_string_key(K, wibble),
        ?assertEqual(
           wibble,
           jiffy_global:get_string_key(K))
    after
        jiffy_global:delete(K)
    end,
    ?assertEqual(
       bar,
       jiffy_global:get_string_key(K, bar)),
    ?assertEqual(
       nil,
       jiffy_global:get_string_key(K)),
    ok.

-endif.
