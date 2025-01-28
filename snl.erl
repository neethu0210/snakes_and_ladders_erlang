-module(snl).
-export([main/0]).

-record(query_entry, {v, dist}).

main() ->
    N = 100,
    Ladders = [{2, 38}, {7, 14}, {8, 31}, {15, 26}, {21, 42}, {28, 84}, {36, 44}, {51, 67}, {71, 91}, {78, 98}],
    Snakes = [{16, 6}, {46, 25}, {49, 11}, {62, 19}, {64, 60}, {74, 53}, {89, 68}, {92, 88}, {95, 75}, {99, 80}],
    Moves = generate_moves(N, Ladders, Snakes),
    MinThrows = get_min_dice_throws(Moves),
    io:format("Minimum dice throws required is: ~p~n", [MinThrows]).

generate_moves(N, Ladders, Snakes) ->
    Moves = lists:duplicate(N, -1),
    MovesWithLadders = lists:foldl(
        fun({Start, End}, Acc) -> update_list(Acc, Start, End) end,
        Moves, Ladders
    ),
    lists:foldl(
        fun({Start, End}, Acc) -> update_list(Acc, Start, End) end,
        MovesWithLadders, Snakes
    ).

update_list(List, Index, Value) ->
    {Left, [_|Right]} = lists:split(Index, List),
    Left ++ [Value] ++ Right.

get_min_dice_throws(Moves) ->
    N = length(Moves),
    Visited = lists:duplicate(N, false),
    Queue = [#query_entry{v = 0, dist = 0}],
    Visited1 = set_visited(Visited, 0),
    bfs(Moves, Visited1, Queue, N).

set_visited(Visited, Index) ->
    Updated = lists:sublist(Visited, 1, Index) ++ [true] ++ lists:sublist(Visited, Index + 2, length(Visited) - Index - 1),
    Updated.

bfs(Moves, Visited, [#query_entry{v = V, dist = Dist} | Tail], N) ->
    case V == N-1 of
        true ->
            Dist;
        false ->
            {NewVisited, NewQueue} = explore_neighbors(V, Dist, Visited, Moves, N, Tail),
            if
                length(NewQueue) > 0 ->
                    bfs(Moves, NewVisited, NewQueue, N);
                true -> Dist
            end

    end.

explore_neighbors(V, Dist, Visited, Moves, N, Tail) ->
    lists:foldl(
        fun(DiceRoll, {VisitedAcc, QueueAcc}) ->
            Target = V + DiceRoll,
            case Target < N andalso not lists:nth(Target+1, VisitedAcc) of
                true ->
                    NewV = case lists:nth(Target+1, Moves) of
                        -1 -> Target;
                        X -> X
                    end,
                    NewVisited = set_visited(VisitedAcc, Target),
                    {NewVisited, QueueAcc ++ [#query_entry{v = NewV, dist = Dist + 1}]};
                false ->
                    {VisitedAcc, QueueAcc}
            end
        end,
        {Visited, Tail},
        lists:seq(1, 6)
    ).
