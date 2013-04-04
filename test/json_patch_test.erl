
-module(json_patch_test).
-include_lib("eunit/include/eunit.hrl").


path_test() ->
    ?assertEqual([<<"test">>] , json_patch:path(<<"/test">>)),
    ?assertEqual([<<"test">>, <<"one">>] , json_patch:path(<<"/test/one">>)),
    ?assertEqual([<<"test">>, <<"one">>, <<"two">>] , json_patch:path(<<"/test/one/two">>)).
    
patch_null_test() ->
    Data = [{<<"test">>, <<"1234">>}],
    ?assertEqual(Data, json_patch:patch([], Data)).
    

patch_test_value_test() ->
    Data = [{<<"test">>, <<"1234">>}],
    Test = [[{<<"op">>, <<"test">>},
	     {<<"path">>, <<"/test">>},
	     {<<"value">>, <<"1234">>}]],
    ?assertEqual(Data, json_patch:patch(Test, Data)),
    ?assertError({badmatch,_}, json_patch:patch(Test, [])),
    ?assertError({badmatch,_}, json_patch:patch(Test, [{<<"test">>, <<"333333">>}])).
    

patch_remove_value_test() ->     
    Data = [{<<"test">>, <<"1234">>}],
    Test = [[{<<"op">>, <<"remove">>},
	     {<<"path">>, <<"/test">>}]],
    ?assertEqual([], json_patch:patch(Test, Data)).

patch_add_value_test() ->
    Data = [{<<"test">>, <<"1234">>}],
    Test = [[{<<"op">>, <<"add">>},
	     {<<"path">>, <<"/test">>},
	     {<<"value">>, <<"1234">>}]],
    ?assertEqual(Data, json_patch:patch(Test, [])).

patch_replace_value_test() ->
    Data1 = [{<<"test">>, <<"1234">>}],
    Data2 = [{<<"test">>, <<"4321">>}],
    Test = [[{<<"op">>, <<"replace">>},
	     {<<"path">>, <<"/test">>},
	     {<<"value">>, <<"4321">>}]],
    ?assertEqual(Data2, json_patch:patch(Test, Data1)).

patch_move_value_test() ->
    Data1 = [{<<"test_before">>, <<"1234">>}],
    Data2 = [{<<"test_after">>, <<"1234">>}],
    Test = [[{<<"op">>, <<"move">>},
	     {<<"from">>, <<"/test_before">>},
	     {<<"path">>, <<"/test_after">>}]],
    ?assertEqual(Data2, json_patch:patch(Test, Data1)).

patch_copy_value_test() ->
    Data1 = [{<<"test_before">>, <<"1234">>}],
    Data2 = [{<<"test_after">>, <<"1234">>},
	     {<<"test_before">>, <<"1234">>}],
    Test = [[{<<"op">>, <<"copy">>},
	     {<<"from">>, <<"/test_before">>},
	     {<<"path">>, <<"/test_after">>}]],
    ?assertEqual(Data2, json_patch:patch(Test, Data1)).



patch_patches_as_json_test() ->
    Data1 = [{<<"test_before">>, <<"1234">>}],
    Patches = <<"[]">>,
    ?assertEqual(Data1, json_patch:patch(Patches, Data1)).

patch_data_as_json_test()->
    Data1 = jsx:encode([{<<"test_before">>, <<"1234">>}]),
    Patches = <<"[]">>,
    ?assertEqual(Data1, json_patch:patch(Patches, Data1)).
    
