-module(simpsonrule).
-compile([export_all]).

compute(Idx, Acc, A, H, Fun) ->
    X = A + Idx * H,
    case Idx rem 2 of
        0 ->
            NewAcc = Acc + apply(Fun, [X]) * 2;
        _ ->
            NewAcc = Acc + apply(Fun, [X]) * 4
    end,
    NewAcc.

cycle(Start, End, Step, Fun, Args, Acc) ->
    NextStep = Start + Step,
    case NextStep > End of 
        true ->
            Acc;
        false ->
            NewAcc = apply(Fun, [Start, Acc|Args]),
            cycle(NextStep, End, Step, Fun, Args, NewAcc)
    end.

cycle(Start, End, Args, Acc) ->
    cycle(Start, End, 1, fun compute/5, Args, Acc).

compute_proc(A, B, Rank, NumOfIter_, NumOfProcs, Fun, Pid) ->
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
    Result = cycle(Start, End, [A, H, Fun], 0.0),

    %% Send the result to the main proc.
    Pid ! {value, Result},

    Result.

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

get_host() ->
    N = node(),
    S = atom_to_list(N),
    Idx = string:chr(S, $@),
    string:substr(S, Idx + 1).

spawn_at_slave({SlaveNum, {ok, Node}}, [A, B |Args]) ->
    spawn(Node, ?MODULE, compute_proc, [A, B, SlaveNum |Args]).

main_proc(A, B, NumOfIter, NumOfProcs, Fun) ->
    Host = get_host(),
    io:format("Hostname: ~s~n", [Host]),

    % spawn slave erlang nodes
    Slaves = lists:map(fun(X) -> 
                               {X, slave:start(Host, 
                                               "slave" ++ integer_to_list(X))}
                       end,
                       lists:seq(1, NumOfProcs - 1)),
    Args = [A, B, NumOfIter, NumOfProcs, Fun, self()],
    lists:foreach(fun(X) -> spawn_at_slave(X, Args) end, Slaves),

    H = (B - A) / NumOfIter,
    compute_proc(A, B, 0, NumOfIter, NumOfProcs, Fun, self()),
    Vals = wait(NumOfProcs, []),

    % stop slave nodes
    lists:foreach(fun(X) -> slave:stop(X) end, nodes()),
    
    R = lists:sum(Vals),
    Result = (R + Fun(B)) * (H/3.0),
    Result.

    

start(_Args) ->
    {NumOfProcs, NumOfIter, A, B} = {2, 100000000, 0.0, 1.0}, 
    R = main_proc(A, B, NumOfIter, NumOfProcs, fun math:sin/1),
    io:format("Result: ~p~n", [R]),
    init:stop(0).


