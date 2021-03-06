-module(astar).
-export([list2dict/1, path/3,
         pmap/5,
         test_dict_initial/0, test_path_initial/0,
         test_dict_nopath/0, test_path_nopath/0,
         test_dict_n/1, test_path_big/0,
         test_dict_diff1/1, test_path_longpath/0]).

-record(vertex, {value, score = 0, estimate, weight, path = []}).

read_concurrency() ->
    case erlang:system_info(compat_rel) > 14 of
        true -> [{read_concurrency, true}];
        false -> []
    end.

list2dict(List) ->
    Dict = ets:new(unnamed, [set, protected] ++ read_concurrency()),
    ets:insert(Dict, [{X} || X <- List]),
    Dict.

path(From, From, _Dict) -> [];
path(From, To, Dict) ->
    ClosedSet = ets:new(unnamed, [set, {keypos, 2}, protected] ++ read_concurrency()),
    OpenSet = ets:new(unnamed, [set, {keypos, 2}, protected] ++ read_concurrency()),
    OpenOSet = ets:new(unnamed, [ordered_set, private]),
    CostEstimate = cost_estimate(From, To),
    Start = #vertex{value = From, estimate = CostEstimate, weight = CostEstimate},
    ets:insert(OpenSet, Start),
    ets:insert(OpenOSet, weightify(Start)),
    Result = find_path(To, ClosedSet, OpenSet, OpenOSet, Dict),
    ets:delete(OpenOSet),
    ets:delete(OpenSet),
    ets:delete(ClosedSet),
    Result.

find_path(To, ClosedSet, OpenSet, OpenOSet, Dict) ->
    case ets:info(OpenSet, size) of
        0 -> no_path;
        _ ->
            case ets:first(OpenOSet) of
                {_, To} ->
                    [X] = ets:lookup(OpenSet, To),
                    lists:reverse([To | X#vertex.path]);
                {_, XV} = OSXV ->
                    [X] = ets:lookup(OpenSet, XV),
                    ets:insert(ClosedSet, X),
                    ets:delete(OpenOSet, OSXV),
                    ets:delete(OpenSet, XV),
                    MainCycle = self(),
                    pmap(fun(TryChar) ->
                                lists:map(fun(YV) ->
                                            case ets:member(ClosedSet, YV) of
                                                true -> skip;
                                                false ->
                                                    Score = X#vertex.score + 1,
                                                    Better = case ets:lookup(OpenSet, YV) of
                                                        [] -> true;
                                                        [Y1] -> Score < Y1#vertex.score
                                                    end,
                                                    case Better of
                                                        true ->
                                                            CostEstimate = cost_estimate(YV, To),
                                                            Weight = Score + CostEstimate,
                                                            Y = #vertex{value = YV, score = Score, estimate = CostEstimate,
                                                                weight = Weight, path = [XV | X#vertex.path]},
                                                            MainCycle ! {better_vertex, Y};
                                                        _ -> skip
                                                    end
                                            end
                                    end,
                                    lists:filter(fun(To1) when To1 == To -> true; (X1) -> ets:member(Dict, X1) end,
                                            [lists:sublist(XV, N - 1) ++ [TryChar] ++ lists:nthtail(N, XV)
                                                || {N, S} <- lists:zip(lists:seq(1, length(XV)), XV), S =/= TryChar]))
                        end, lists:seq($a, $z)),
                    add_new_vertexes(OpenSet, OpenOSet),
                    find_path(To, ClosedSet, OpenSet, OpenOSet, Dict)
            end
    end.

add_new_vertexes(OpenSet, OpenOSet) ->
    receive
        {better_vertex, #vertex{value = YV, score = Score} = Y} ->
            {Better, OldWeight} = case ets:lookup(OpenSet, YV) of
                [] -> {true, nonexistent};
                [Y1] -> {Score < Y1#vertex.score, Y1#vertex.weight}
            end,
            case Better of
                true ->
                    ets:insert(OpenSet, Y),
                    ets:delete(OpenOSet, {OldWeight, Y#vertex.value}),
                    ets:insert(OpenOSet, weightify(Y));
                _ -> skip
            end,
            add_new_vertexes(OpenSet, OpenOSet);
        {pmap_done, _} -> done
    end.

weightify(#vertex{value = Value, weight = Weight}) -> {{Weight, Value}}.

%neighbors(Word, Dict, To) ->
%    lists:filter(fun(To1) when To1 == To  -> true; (X1) -> ets:member(Dict, X1) end, lists:flatmap(fun(X) -> X end,
%        [[lists:sublist(Word, N - 1) ++ [Alphabet] ++ lists:nthtail(N, Word)
%                || Alphabet <- lists:seq($a, $z), Alphabet =/= S]
%            || {N, S} <- lists:zip(lists:seq(1, length(Word)), Word)])).

cost_estimate(From, To) ->
    lists:foldl(fun({X, X}, A) -> A; ({_, _}, A) -> A + 1 end, 0, lists:zip(From, To)).

pmap(F, List) ->
    spawn_link(?MODULE, pmap, [F, List, 0, sending, self()]).

pmap(_, Res, 0, awaiting, Boss) ->
    Boss ! {pmap_done, Res};
pmap(_, Res, N, awaiting, Boss) ->
    receive
        {res, El} -> pmap(nothing, [El | Res], N - 1, awaiting, Boss)
    end;
pmap(_, [], N, sending, Boss) ->
    pmap(nothing, [], N, awaiting, Boss);
pmap(F, [El | List], N, sending, Boss) ->
    X = self(),
    spawn_link(fun() -> X ! {res, F(El)} end),
    pmap(F, List, N + 1, sending, Boss).

test_dict_initial() ->
    Dict = ets:new(unnamed, [set, protected] ++ read_concurrency()),
    ets:insert(Dict, [{"start"}, {"stark"}, {"stack"}, {"slack"}, {"bluck"}, {"black"},
            {"blank"}, {"bland"}, {"aland"}, {"cland"}, {"brand"}, {"braid"}]),
    Dict.

test_path_initial() ->
    io:format("~p~n", [path("smart", "brain", test_dict_initial())]).

test_dict_nopath() ->
    Dict = ets:new(unnamed, [set, protected] ++ read_concurrency()),
    ets:insert(Dict, [{"start"}, {"stark"}, {"stack"}, {"slack"}, {"eluck"}, {"elack"},
            {"blank"}, {"bland"}, {"aland"}, {"cland"}, {"brand"}, {"braid"}]),
    Dict.

test_path_nopath() ->
    io:format("~p~n", [path("smart", "brain", test_dict_nopath())]).

test_dict_n(N) ->
    [[random:uniform(26) + $a - 1 || _ <- lists:seq(1,5)] || _ <- lists:seq(1, N)].

test_path_big() ->
    N = 500000,
    {Megasecs1, Secs1, Microsecs1} = now(),
    List = test_dict_n(N),
    Dict = list2dict(List),
    {Megasecs2, Secs2, Microsecs2} = now(),
    io:format("~p random words (like ~p, ~p, ~p...), ~.3f secs to generate~n",
        [N, lists:nth(1, List), lists:nth(2, List), lists:nth(3, List),
        (Megasecs2 * 1000000 + Secs2 + Microsecs2 / 1000000 - Megasecs1 * 1000000 - Secs1 - Microsecs1 / 1000000)]),
    [
        fun() ->
                {Megasecs3, Secs3, Microsecs3} = now(),
                [From, To] = test_dict_n(2),
                Res = path(From, To, Dict),
                {Megasecs4, Secs4, Microsecs4} = now(),
                io:format("from ~p to ~p:~n    ~p, ~.3f secs~n",
                    [From, To, Res,
                        (Megasecs4 * 1000000 + Secs4 + Microsecs4 / 1000000 - Megasecs3 * 1000000 - Secs3 - Microsecs3 / 1000000)])
        end() || _ <- lists:seq(1, 100)],
    done.

test_dict_diff1(W) ->
    N = random:uniform(5),
    lists:sublist(W, N - 1) ++ [random:uniform(26) + $a - 1] ++ lists:nthtail(N, W).

test_path_longpath() ->
    [From] = test_dict_n(1),
    List = lists:foldl(fun(_, [H | _]=A) -> [test_dict_diff1(H) | A] end, [From], lists:seq(1,20)),
    Dict = list2dict(List),
    [To | _] = List,
    io:format("path(~p, ~p, ~p) = ~p~n", [From, To, List, path(From, To, Dict)]).

