%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           Author: Ethan Edge                                 %
%                                                                              %
%    Has a predicate hand_value/3 that takes a hand and a starter card         %
%    and gives you the value of that hand according to the rules of Cribbage.  %
%    Also has a predicate select_hand/3 that tries to select the best hand.    %
%                                                                              %          
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This program intends to facilitate two parts of the card game Cribbage.
%
% The first part is scoring a hand with the hand_value/3 predicate. It scores 
% the hand based on 5 scoring criteria. 
%
% 1. 15s which is 2 points for each distinct ombination of cards that add to 15. 
% Ace is 1 and jack, queen and king are 10.
%
% 2. Pairs which gives 2 points for each pair. It ensures that things like 4 of 
% a kind get their respective 6 pairs.
%
% 3. Runs, 1 point for each card in a run of 3 or more consecutitive card.
%
% 4. Flushes, 4 points if all the cards in the hand are the same suit and an
% extra point if the start card also is of the same suit.
%
% 5. One for his nob, 1 point if the hand has a jack of the same suit as the 
% start card.
%
% This main predicate calls some helper predicates to get various information on
% the hand as well as a separate predicate for each scoring criteria.
%
% The second part is choosing the best possible hand from 5-6 cards without
% knowing the start card. This is achieved with the select_hand/3 predicate.
% The strategy used here is to generate all the possible hands and then score
% them along with every possible start card and taking the average score. Then
% we have chosen the hand that is expected to maximise our value.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%     SECTION 0: DATA TYPES AND FACTS     %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% card(Rank, Suit)
%
% A card/2 data type that has a Rank and a Suit.
card(2, hearts). card(3, hearts). card(4, hearts). card(5, hearts). 
card(6, hearts). card(7, hearts). card(8, hearts). card(9, hearts).
card(jack, hearts). card(queen, hearts). card(king, hearts). card(ace, hearts).

card(2, diamonds). card(3, diamonds). card(4, diamonds). card(5, diamonds). 
card(6, diamonds). card(7, diamonds). card(8, diamonds). card(9, diamonds).
card(jack, diamonds). card(queen, diamonds). card(king, diamonds). 
card(ace, diamonds).

card(2, spades). card(3, spades). card(4, spades). card(5, spades). 
card(6, spades). card(7, spades). card(8, spades). card(9, spades).
card(jack, spades). card(queen, spades). card(king, spades). card(ace, spades).

card(2, clubs). card(3, clubs). card(4, clubs). card(5, clubs). 
card(6, clubs). card(7, clubs). card(8, clubs). card(9, clubs).
card(jack, clubs). card(queen, clubs). card(king, clubs). card(ace, clubs).


% value(Suit, Value)
%
% Simply establishes facts about each Suits' value.
value(1,1). value(2,2). value(3,3). value(4,4). value(5,5).  
value(6,6). value(7,7). value(8,8). value(9,9). value(10,10).
value(ace,1). value(jack,10). value(king,10). value(queen,10).


% run_value(Suit, Value)
%
% Establishes facts about each Suits' Value (precedence) in a run.
run_value(1,1). run_value(2,2). run_value(3,3). run_value(4,4). 
run_value(5,5). run_value(6,6). run_value(7,7). run_value(8,8). run_value(9,9). 
run_value(10,10). run_value(ace,1). run_value(jack,11). run_value(king,13). 
run_value(queen,12).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%     SECTION 1: HAND VALUE PREDICATE      %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% The main hand_value/3 predicate which scores a card hand based on the rules
%%% of Cribbage along with all its helper predicates.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   1.1 - hand_value/3   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% hand_value(+Hand, +Startcard, -Value)
%
% Takes in Hand and Startcard and gives us the value of this Hand, Value.
% Uses some helper predicates to get certain information from the cards like 
% their Ranks, SortedValues and RankCounts. Then uses predicates for each 
% scoring rule to get their respective scores R1-R5 and sums these.
hand_value(Hand, Startcard, Value) :-
    
    extract_ranks([Startcard|Hand], Ranks),   
    sorted_values(Ranks, SortedValues),
    rank_count(Ranks, RankCounts),
    
    fifteens(SortedValues, R1),    
    pairs(RankCounts, R2),
    runs(RankCounts, R3),
    flushes_main(Hand, R4),
    flushes_extra(Hand, Startcard, R4Extra),
    one_for_his_nob(Hand, Startcard, R5),
    
    Value is R1 + R2 + R3 + R4 + R4Extra + R5.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   1.2 - Common Helper Predicates   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% extract_ranks(+Hand, -Ranks)
%
% Takes Hand and gives us each card's rank in Ranks.
extract_ranks([], []).
extract_ranks([card(R,_)|Hs], [R|Rs]) :-
    extract_ranks(Hs, Rs).


% sorted_values(+Hand, -SortedValues)
%
% Takes Ranks and gives us the corresponding SortedValues. Values based on the
% value/2 fact.
sorted_values(Ranks, SortedValues) :-               
    map_values(Ranks, Values),
    msort(Values, SortedValues).


% map_values(+Ranks, -Values)
%
% Takes Ranks and maps each rank to their value in Values based on value/2.
map_values([], []).
map_values([R|Rs], [V|Vs]) :-
    map_values(Rs, Vs),
    value(R, V).


% rank_count(+Ranks, -RankCounts)
%
% Takes in a list of Ranks giving a list of rank (R), count (C) pairs in
% RankCounts.
rank_count([],[]).
rank_count([R|Rs], [R-C|Cs]) :-
    aggregate_all(count, member(R, [R|Rs]), C),
    % Deletes all R before re-calling rank_counts/2 to avoid duplicates.
    delete(Rs, R, Ps),
    rank_count(Ps, Cs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   1.3 - Fifteens Scoring and Helper Predicates   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fifteens(+SortedValues, -R1)
%
% Takes a list of SortedValues and gives us a score (R1). 2 points for
% every combination of values that sum to 15.
fifteens(SortedValues, R1) :-
    combs(SortedValues, Combs),
    sum_to_15(Combs, R1List),
    sum_list(R1List, R1).


% sum_to_15(+Combs, -List)
%
% Takes Combs, a list of lists of numbers and gives us List, which holds the
% points to sum up to get the score for 15s (R1).
sum_to_15(Combs, List) :-
    sum_to_15(Combs, [], List).
                   
% sum_to_15(+Combs, +Acc, -List)
%
% For each C in Combs, if the values sum to 15, 2 points are added to List, 
% otherwise 0 is.
sum_to_15([], List, List).
sum_to_15([C|Cs], Acc, List) :-
    sum_list(C, Sum),
    (Sum =:= 15 ->
    V = 2
    ; V = 0),    
    sum_to_15(Cs, [V|Acc], List).


% comb(+List, ?Comb)
%
% Holds if Comb is a combination of elements in List. List and Comb should be
% sorted.
% Either the two lists have the same head and their tails are combs or they have
% different heads and the whole of Comb is a comb of List's tail.
comb([],[]).
comb([H|Hs],[H|Hs2]) :-
    comb(Hs, Hs2).
comb([_|Hs], Hs2) :-
    comb(Hs, Hs2).


% combs(+Orig, -Comb)
%
% Generates a list (Combs) of all the different combinations of Orig, minus the
% empty list.
combs(Orig, Combs) :-
    bagof(PartComb, comb(Orig, PartComb), Comb1),
    delete(Comb1, [], Combs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   1.4 - Pairs Scoring and Helper Predicates   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% pairs(+Pairs, -R2)
%
% Takes in Pairs, a list of pairs R-C, where R is the card rank and C is the 
% suit and calculates the amount of pairs multiplied by *2 for the score (R2).
% Checks each pair from Pair recursively. If there are at least 2 cards for a
% given rank, the amount of possible pairs is calculated with the n_choose_k
% predicate and multiplied by 2 for the score. Otherwise 0 is added.

pairs([], 0).
pairs([_-C|Rs], R2) :-
    (C >= 2 ->
    n_choose_k(C, 2, Combs)
    ;
    Combs = 0    
    ),
    pairs(Rs, NowR2),
    R2 is (Combs * 2) + NowR2.


%% n_choose_k(+N, +K, -C)
%
% Takes N, K and calculates N choose K, giving C.
n_choose_k(N, K, C) :-
    fact(N, NFact),
    fact(K, KFact),
    NK is N - K,
    fact(NK, NKFact),
    C is NFact / (KFact * NKFact).


% fact(+N, -F)
%
% Takes a number N and gives factorial F by using fact/3
fact(N, F) :-
    fact(N, 1, F).

% fact(+N, +A, -F)
%
% Takes a number N and an accumulator A (intiailly 1) and gives the factorial F
fact(N, A, F) :-
    (N =:= 0 ->
      F = A
    ; N > 0,
      N1 is N - 1,
      A1 is N * A,
      fact(N1, A1, F)
    ).        


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   1.5 - Runs Scoring and Helper Predicates   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% runs(+RankCounts, -R3)
%
% Takes in RankCounts and gives us R3, the total score of the runs.
% Generates all runs of length 3+, ensures they are runs by definition and then
% only takes the longest runs (reasoning in true_runs/2) before simply scoring
% the runs. 
runs(RankCounts, R3) :-
    combs_least_3(RankCounts, Combs),
    are_runs(Combs, Runs),
    true_runs(Runs, TRuns),
    score_runs(TRuns, R3List),
    sumlist(R3List, R3).


% combs_least_3(+Orig, -Combs)
%
% Generates a list (Combs) of all the different combinations of at least length
% 3.
combs_least_3(Orig, Combs) :-
    combs(Orig, Combs1),
    length_3_plus(Combs1, Combs).


% length_3_plus(+List1, -List2)
%
% Takes List1 and gives us List2 which has all sub-lists from List1 that are 3
% or more elements long.
length_3_plus([],[]).
length_3_plus([L1|L1s],[L1|L2s]) :-
    length(L1, Len),
    Len >= 3,
    length_3_plus(L1s, L2s).
length_3_plus([L1|L1s],L2s) :-
    length(L1, Len),
    Len < 3,
    length_3_plus(L1s, L2s).


% are_runs(+Combs, -Runs)
%
% Takes in Combs and returns Runs, a list of the sub-lists from Combs that are
% runs. Takes each C in Combs and transforms it into a sorted list of precendece
% values and decides if it's a a run with is_run/1.
are_runs([], []).
are_runs([C|Cs], [C|Rs]) :-
    map_run_values(C, CMapped),
    keysort(CMapped,CSorted), 
    is_run(CSorted),
    are_runs(Cs, Rs).
are_runs([C|Cs], Rs) :-    
    map_run_values(C, CMapped),
    keysort(CMapped, CSorted), 
    \+ is_run(CSorted),
    are_runs(Cs, Rs).


% map_run_values(+Ranks, -Values)
%
% Takes Ranks and maps each rank to their value in Values based on run_value/2.
map_run_values([], []).
map_run_values([R-C|Rs], [V-C|Vs]) :-
    map_run_values(Rs, Vs),
    run_value(R, V).


% is_run(Comb)
%
% Takes Comb, a list of pairs V-C, where V is a value and C is a count of that
% value. Holds if Comb is a run, based on the values.
% Recursively checks adjacent pairs to ensure the second value is one more than
% the previous.
is_run([_]).
is_run([R1-_, R2-_|Rs]) :-
    Step is R1 + 1,
    R2 =:= Step,
    is_run([R2-_|Rs]).


% true_runs(+Runs, -TrueRuns)
%
% Takes a Runs and gives TrueRuns, a list of the longest runs.
%
% This is based on the assumptions that runs must be of length 3 and once we
% combine a Hand with a StartCard, we have a total of 5 cards. So there can only
% be runs of either lengths 3, 4 or 5. Not sets of runs with multiple lengths.
% Example: Two runs of length 3 and one of 4 would imply that the length 3 runs 
% are subsets of the length 4 and thus aren't counted.
true_runs([], []).
true_runs(Runs, TrueRuns) :-
    maplist(length, Runs, Lens),
    max_list(Lens, M),
    keep_longest(Runs, M, TrueRuns).


% keep_longest(+List, +M, -Longest)
%
% Takes List and gives Longest which holds all the sub-lists of List that are of
% length M.
keep_longest([], _, []).
keep_longest([C|Cs], M, [C|Ms]) :-
    length(C, L),
    L =:= M,
    keep_longest(Cs, M, Ms).
keep_longest([C|Cs], M, Ms) :-
    length(C, L),
    L =\= M,
    keep_longest(Cs, M, Ms).


% score_runs(+Runs, -R3List)
%
% Takes in Runs and scores each run in it and puts the score in R3List.
score_runs(Runs, R3List):-
    score_runs(Runs, [], R3List).

% score_runs(+Runs, +Acc, -R3List)
%
% Scores each run by its length multiplied by how many runs of that length are
% possible with those values. Example: we have values 3,4,5,3. Therefore we have
% runs 3,4,5 and 3,4,5 (since 3 appears twice) which has score 3 * 2 = 6.
score_runs([], R3List, R3List).
score_runs([R|Rs], PrevR3List, R3List) :-
    length(R, L),
    num_runs(R, M),
    P is L * M,
    score_runs(Rs, [P|PrevR3List], R3List).


% num_runs(+Run, -N)
%
% Takes a Run and gives us N which is how many combinations of cards make that
% exact run. Look at example in score_runs/3.
num_runs(Run, N) :-
    num_runs(Run, 1, N).

% num_runs(+Run, +Acc, -N)
%
% Counts the number of ways that Run can be made by taking an initial Acc of 1
% and multiplying it by the counts (C) of each rank in the Run. Example:
% [3-2, 4-1, 5-1] is the run 3,4,5 but can be made two ways given there's two 3s
num_runs([], N, N).
num_runs([_-C|Rs], A, N) :-
    A1 is A * C,
    num_runs(Rs, A1, N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   1.6 - Flushes Scoring and Helper Predicates   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% flushes_main(+Hand, +Startcard, -R4Extra)
%
% Calculates the score for flushes.
flushes_main(Hand, 4) :-
    same_suits(Hand, _).
flushes_main(Hand, 0) :-
    different_suits(Hand, _).


%% flushes_extra(+Hand, +Startcard, -R4Extra)
%
% Calculates the extra point for flushes if the Startcard also has the same 
% Suit as the Hand.
flushes_extra(Hand, Startcard, 1) :-
    same_suits([Startcard|Hand], _).
flushes_extra(Hand, Startcard, 0) :-
    different_suits([Startcard|Hand], _).


% same_suit(+Hand, +Suit2)
%
% Holds if all card(Rank, Suit)s in Hand have the same Suit.
same_suits([], _).
same_suits([card(_,Suit)|Hs], Suit) :-
    same_suits(Hs, Suit).


% different_suit(+Hand, +Suit2)
%
% Holds if at least one card(Rank, Suit) in Hand has a different Suit to Suit2.
different_suits([card(_,Suit)|_], Suit2) :-
    Suit \= Suit2.    
different_suits([card(_,Suit)|Hs], Suit) :-
    different_suits(Hs, Suit).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   1.7 - One For His Nob Scoring   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% one_for_his_nob(+Hand, +Startcard, +Prev5, -R5)
%
% Considers Hand and Startcard for the "One for his nob" point, giving R5.
%
% Checks to see if Hand has a card(jack, Suit) where the Suit matches the 
% StartCard. If the match is found, R5 is bound to 1 and a cut prevents any 
% further recursion. Otherwise, R5 is set to 0.
one_for_his_nob([], _, R5) :-
    R5 is 0.
one_for_his_nob([card(jack, Suit)|_], card(_, Suit), R5) :-
    R5 is 1, !.    
one_for_his_nob([_|Hs], card(_, Suit), R5) :-
    one_for_his_nob(Hs, card(_, Suit), R5).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%     SECTION 2: SELECT HAND PREDICATE      %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% The main select_hand/3 predicate which chooses the best hand from 5-6 cards
%%% based on the rules of Cribbage, along with its helper predicates.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   2.1 - select_hand/3   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% select_hand(+Cards, -Hand, -Cribcards)
%
% Takes in Cards, a list of card/2s, and gives us a Hand based on trying to 
% maximise our potential score. Also gives Cribcards, the leftover cards.
%
% Attempts to choose the best Hand by simply generating all the potential hands
% of length 4, scoring that Hand with every possible starting card, averaging
% those scores and then choosing the Hand with the highest average score.

select_hand(Cards, Hand, Cribcards) :-
    combs_len_4(Cards, Combs),
    avg_score_hands(Combs, Scores),
    % Sort and reverse so head of list has the highest score
    keysort(Scores, Sorted),
    reverse(Sorted, [_-Hand|_]),
    subtract(Cards, Hand, Cribcards).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  2.2 - The Helper Predicates   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% combs_len_4(+List, -Combs)
%
% Takes List and produces Combs, a list of all the different combinations of
% elements in List. Each combination is 4 in length.
combs_len_4(List, Combs) :-
    combs(List, C),
    length_4(C, Combs).


% avg_score_hands(+Hands, -Scored)
%
% Takes a list (Hands) of different hand combinations, creates a pair between
% that each hand's AvgScore and the hand (H) and puts into Scored.
%
% Uses bagof/3 to create a list, HandScores, of every score possible for that
% hand depending on the chosen StartCard. Takes the average of HandScores and
% gives H that score.
avg_score_hands([], []).
avg_score_hands([H|Hs], [AvgScore-H|AvgScores]) :-    
    
    % Ensures the card(X, Y) exists and is allowed to be a StartCard before it's
    % passed into hand_value/3 with H to be scored.
    bagof(S, X^Y^(card(X,Y), \+ member(card(X,Y), H), 
                 hand_value(H, card(X, Y), S)), HandScores),
    
    sumlist(HandScores, Sum),
    length(HandScores, Len),    
    AvgScore is Sum / Len,
    avg_score_hands(Hs, AvgScores).


% length_4(+Lists1, -Lists2)
%
% Takes a list of lists (List1) and gives Lists2 which only has the lists from
% Lists1 that are of length 4.
length_4([],[]).
length_4([L1|L1s],[L1|L2s]) :-
    length(L1, Len),
    Len =:= 4,
    length_4(L1s, L2s).
length_4([L1|L1s],L2s) :-
    length(L1, Len),
    Len =\= 4,
    length_4(L1s, L2s).
