-module(simpsonrule).
-compile([export_all]).

-define(FUN(X), math:sin(X)).

-spec(compute(integer(), float(), float(), float()) -> float()).
compute(Idx, Acc, A, H) ->
    case Idx rem 2 of
        0 ->
            NewAcc = Acc + ?FUN(A + Idx * H) * 2;
        _ ->
            NewAcc = Acc + ?FUN(A + Idx * H) * 4
    end,
    NewAcc.

-spec(cycle(integer(), integer(), list(), float()) -> float()).
cycle(Start, End, Args=[A, H], Acc) ->
    NextStep = Start + 1,
    case NextStep > End of 
        true -> Acc;
        false ->            
            NewAcc = compute(Start, Acc, A, H),
            cycle(NextStep, End, Args, NewAcc)
    end.

-spec(compute_proc(float(), float(), integer(), integer(), integer(), pid()) 
      -> float()).
compute_proc(A, B, Rank, NumOfIter_, NumOfProcs, Pid) ->
    NumOfIter = NumOfIter_ + (NumOfIter_ rem 2),
    LocalNumIter = NumOfIter div NumOfProcs, 
    Start = Rank * LocalNumIter,
    End_ = Rank * LocalNumIter + LocalNumIter,
    if 
        Rank =:= NumOfProcs - 1 ->
            End = End_ + (NumOfIter rem NumOfProcs);
        true ->
            End = End_
    end,
    io:format("Proc[~p]: iterating from ~p to ~p~n", [Rank, Start, End]),   
    H = (B - A) / NumOfIter,
    Result = cycle(Start, End, [A, H], 0.0),

    %% Send the result to the main proc.
    Pid ! {value, Result},

    Result.

-spec(wait(integer(), [float()]) -> [float()]).
wait(0, Vals) ->
    Vals;
wait(N, Vals) ->
    receive
        {value, Val} ->
            wait(N-1, [Val|Vals]);
        _ ->
            wait(N, Vals)
    after 
        10000 ->  % Case something brakes 
            Vals
    end.

-spec(get_host() -> [integer()]).
get_host() ->
    N = node(),
    S = atom_to_list(N),
    Idx = string:chr(S, $@),
    string:substr(S, Idx + 1).

-spec(spawn_at_slave({integer(), {atom(), atom()}}, [any()]) -> pid()).
spawn_at_slave({SlaveNum, {ok, Node}}, [A, B |Args]) ->
    spawn(Node, ?MODULE, compute_proc, [A, B, SlaveNum |Args]).

-spec(main_proc(float(), float(), integer(), integer()) -> float()).
main_proc(A, B, NumOfIter, NumOfProcs) ->
    Host = get_host(),
    io:format("Hostname: ~s~n", [Host]),

    % spawn slave erlang nodes
    Slaves = lists:map(fun(X) -> 
                               {X, slave:start(Host, 
                                               "slave" ++ integer_to_list(X))}
                       end,
                       lists:seq(1, NumOfProcs - 1)),
    Args = [A, B, NumOfIter, NumOfProcs, self()],
    lists:foreach(fun(X) -> spawn_at_slave(X, Args) end, Slaves),

    H = (B - A) / NumOfIter,
    compute_proc(A, B, 0, NumOfIter, NumOfProcs, self()),
    Vals = wait(NumOfProcs, []),

    % stop slave nodes
    lists:foreach(fun(X) -> slave:stop(X) end, nodes()),
    
    R = lists:sum(Vals),
    Result = (R + ?FUN(B)) * (H/3.0),
    Result.

    
-spec(start() -> any()).
start() ->
    {NumOfProcs, NumOfIter, A, B} = {2, 100000000, 0.0, 10.0}, 
    R = main_proc(A, B, NumOfIter, NumOfProcs),
    io:format("Result: ~p~n", [R]),
    init:stop(0).


