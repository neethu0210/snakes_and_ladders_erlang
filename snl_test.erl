-module(snl_test).
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {
        setup, 
        fun() -> ok end,
        fun(_) -> ok end,
        [
            {
                "TestCase 1: No ladders or snakes (pure dice rolling)",
                fun() ->
                    Moves = snl:generate_moves(100, [], []),
                    ?assertEqual(17, snl:get_min_dice_throws(Moves))
                end
            },
            {
                "TestCase 2: Single ladder to instant win",
                fun() ->
                    Moves = snl:generate_moves(100, [{2, 99}], []),
                    ?assertEqual(1, snl:get_min_dice_throws(Moves))
                end
            },
            {
                "TestCase 3: Single snake near the end sends back to start",
                fun() ->
                    Moves = snl:generate_moves(100, [], [{98, 2}]),
                    ?assertEqual(17, snl:get_min_dice_throws(Moves))
                end
            },
            {
                "TestCase 4: Ladder from start (Position 0 to 50)",
                fun() ->
                    Moves = snl:generate_moves(100, [{0, 50}], []),
                    ?assertEqual(9, snl:get_min_dice_throws(Moves))
                end
            },
            {
                "TestCase 5: Mixed ladders and snakes setup",
                fun() ->
                    Ladders = [{2, 38}, {7, 14}, {8, 31}, {15, 26}, {21, 42}, {28, 84}, {36, 44}, {51, 67}, {71, 91}, {78, 98}],
                    Snakes = [{16, 6}, {46, 25}, {49, 11}, {62, 19}, {64, 60}, {74, 53}, {89, 68}, {92, 88}, {95, 75}, {99, 80}],
                    Moves = snl:generate_moves(100, Ladders, Snakes),
                    ?assertEqual(7, snl:get_min_dice_throws(Moves))
                end
            }
        ]
    }.