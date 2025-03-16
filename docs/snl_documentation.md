# Snakes and Ladders Solver Documentation

This document provides a detailed explanation of the **Snakes and Ladders BFS Solver**, including the purpose, algorithm, and function breakdown.  

## **Purpose**
This Erlang program solves the **Snakes and Ladders game** using the **Breadth-First Search (BFS)** algorithm. It determines the **minimum number of dice rolls** required to reach the last square while considering the effects of ladders and snakes.

- **Ladders** help the player advance **up** the board.
- **Snakes** move the player **down** the board.
- BFS is used to **find the shortest path** from start to finish.

## **How It Works**
1. **Represent the board** as a list where each index corresponds to a square.
2. **Ladders and snakes** are stored as mappings (`Start → End`).
3. **BFS is applied**, treating the game as an unweighted graph traversal.

## **Records**
```erlang
-record(query_entry, {v :: pos_integer(), dist :: non_neg_integer()}).
```
**v**: current position on board  
**dist**: number of dice rolls taken to reach v  

## **Function *generate_moves/3***
This function initializes the Snakes and Ladders board and applies ladder and snake mappings.  

**Inputs**:  
- **N**: number of squares on the board
- **Ladders**: list of *{Start, End}* tuples representing ladder positions
- **Snakes**: list of *{Start, End}* tuples representing snake positions  

**Output**:
A list of size *N* where each index represents a square. If a ladder or snake is present, the value at that index represents the destination square.  

**Implemenation**:
```erlang
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
```
**Explanation**:  
- **Initialize the board (Moves)**: Create a list of size N where all values are -1, meaning no special moves exist initially.
- **Apply ladders**: Iterate over the Ladders list and update the corresponding index in Moves.
- **Apply snakes**: Iterate over the Snakes list and update the corresponding index in Moves.

## **Function *update_list/3***
This function updates an element in a list at a specified index.  

**Inputs**:  
- **List**: the original list
- **Index**: position to update
- **Value**: new value to set at *Index*  

**Output**:
Updated list with *Value* inserted at *Index*.  

**Implemenation**:
```erlang
update_list(List, Index, Value) ->
    {Left, [_|Right]} = lists:split(Index, List),
    Left ++ [Value] ++ Right.
```
**Explanation**:  
- **Split the list at *Index***, separating it into *Left* (before the index) and *Right* (after the index).
- **Replace the existing value** at *Index* with *Value*.
- **Concatenate the updated parts** back into a single list.

## **Function *get_min_dice_throws/1***
Uses BFS to find the shortest path to the last square.  

**Inputs**:  
- **Moves**: Board representation with ladders and snakes.  

**Implemenation**:
```erlang
get_min_dice_throws(Moves) ->
    N = length(Moves),
    Visited = lists:duplicate(N, false),
    StartV = case lists:nth(1, Moves) of
        -1 -> 0;  
        X -> X  
    end,
    Queue = [#query_entry{v = StartV, dist = 0}],
    Visited1 = set_visited(Visited, StartV),
    bfs(Moves, Visited1, Queue, N).
```
**Explanation**:  
- ***N*** is the total number of squares. 
- ***Visited*** is a boolean list to track visited positions.
- Checks if the *first square* has a ladder. If so, the starting position is updated to the ladder's end.
- **Queue** starts with *StartV* (first square) and *dist = 0*.
- **Marks** *StartV* **as visited**.
- Calls *bfs/4* to perform **BFS**.

## **Function *set_visited/2***
Marks a board position as visited.  

**Inputs**:  
- **Visited**: Boolean list of visited positions.
- **Index**: Position to mark.  

**Implemenation**:
```erlang
set_visited(Visited, Index) ->
    update_list(Visited, Index, true).  
```
**Explanation**:  
- Calls *update_list/3* to mark the *Index* as visited by updating the list.

## **Function *bfs/4***
Executes BFS to find the shortest path.  

**Inputs**:  
- **Moves**: the board representation.
- **Visited**: boolean list tracking visited squares.
- **Queue**: BFS queue
- **N**: total number of squares  

**Implemenation**:
```erlang
bfs(Moves, Visited, [#query_entry{v = V, dist = Dist} | Tail], N) ->
    case V == N-1 of
        true -> Dist;
        false ->
            {NewVisited, NewQueue} = explore_neighbors(V, Dist, Visited, Moves, N, Tail),
            if length(NewQueue) > 0 -> bfs(Moves, NewVisited, NewQueue, N); 
               true -> Dist
            end
    end.
```
**Explanation**:  
- **Check if the current position (*V*) is the last square (*N-1*):**. f yes, return *Dist* (the number of dice rolls taken so far).
- **If not at the last square**, call *explore_neighbors/6* to find the next possible positions.
- **Update *Visited* and the BFS queue** with the newly explored positions.
- **Recursively call *bfs/4*** with the updated queue until the last square is reached.

## **Function *explore_neighbours/6***
Generates possible next moves for BFS by rolling a dice (values 1 to 6) and updating the queue. 

**Inputs**:  
- **V**: current position.
- **Dist**: dice throws so far.
- **Visited**: visited positions.
- **Moves**: board representation. 
- **N**: total squares.
- **Tail**: remaining BFS queue.  

**Implemenation**:
```erlang
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
```
**Explanation**:  
- **Loop through all possible dice rolls (1 to 6)** using *lists:foldl*.
- **Calculate the target square (*Target*)** by adding *DiceRoll* to the current position (*V*).
- **Check if *Target* is within bounds and not visited**:
    - If the target is valid, determine the final destination *(NewV)*:
        - If the target square has a ladder/snake (*Moves[Target] != -1*), move to that position.
        - Otherwise, stay at *Target*.
- **Mark *Target* as visited** using *set_visited/2*.
- **Add the new position to the queue** with an incremented distance (*Dist + 1*).
- **Return the updated queue and visited list** to *bfs/4*.

## **Summary**
1. *generate_moves/3*: Creates a board representation.
2. *update_list/3*: Modifies the board list.
3. *get_min_dice_throws/1*: Finds the shortest path using BFS.
4. *set_visited/2*: Marks positions as visited.
5. *bfs/4*: Implements BFS to find the solution.
6. *explore_neighbors/6*: Determines possible moves.  

This approach efficiently finds the shortest path in the game using **BFS traversal**.