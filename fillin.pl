#!/usr/bin/env swipl
% You can use this code to get your started with your fillin puzzle solver.

:- ensure_loaded(library(clpfd)).

main(PuzzleFile, WordlistFile, SolutionFile) :-
    read_file(PuzzleFile, Puzzle),
    read_file(WordlistFile, Wordlist),
    valid_puzzle(Puzzle),
    solve_puzzle(Puzzle, Wordlist, Solved),
    print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

read_lines(Stream, Content) :-
    read_line(Stream, Line, Last),
    (   Last = true
    ->  (   Line = []
        ->  Content = []
        ;   Content = [Line]
        )
    ;  Content = [Line|Content1],
        read_lines(Stream, Content1)
    ).

read_line(Stream, Line, Last) :-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Line = [],
        Last = true
    ; Char = '\n'
    ->  Line = [],
        Last = false
    ;   Line = [Char|Line1],
        read_line(Stream, Line1, Last)
    ).

print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(same_length(Row), Rows).


% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.
%
% This code is obviously wrong: it just gives back the unfilled puzzle
% as result.  You'll need to replace this with a working
% implementation.

solve_puzzle(Puzzle, Wordlist, PuzzleSolution) :-
    puzzle_to_vars(Puzzle, PuzzleWithVar),
    solve_varpuzzle(PuzzleWithVar, Wordlist, PuzzleSolution).

solve_varpuzzle(PuzzleWithVar, [], PuzzleWithVar).
solve_varpuzzle(PuzzleWithVar, Wordlist, PuzzleWithVar) :-
    puzzle_to_slots(PuzzleWithVar, Slots),
    possible_words(Slots, Wordlist, PossibleSolution),
    zip(Slots, PossibleSolution, AllSlotSolutions),
    filter(has_solutions, AllSlotSolutions, SlotSolution),
    head(SlotSolution, Head),
    find_min_slot(SlotSolution, Head, MinSlot-MinSlotSol),
    try_solution(MinSlot, MinSlotSol, Wordlist, NewWordlist),
    solve_varpuzzle(PuzzleWithVar, NewWordlist, PuzzleWithVar).

%% Setting up
puzzle_to_vars(Puzzle, PuzzleWithVar) :-
    replace_empty_puzzle(Puzzle, PuzzleWithVar).

puzzle_to_slots(PuzzleWithVar, Slots) :-
    rows_to_slots(PuzzleWithVar, RowSlots),
    transpose(PuzzleWithVar, VPuzzleWithVar),
    rows_to_slots(VPuzzleWithVar, VerticleSlots),
    append(RowSlots, VerticleSlots, Slots).

rows_to_slots([], []).
rows_to_slots([E|Es], Slots) :-
    get_slots_for_row(E, RowSlots),
    rows_to_slots(Es, NewSlots),
    append(RowSlots, NewSlots, Slots).

replace_empty_puzzle(Puzzle, PuzzleWithVar) :-
    map(row_to_lgc_var, Puzzle, PuzzleWithVar).

row_to_lgc_var([], []).
row_to_lgc_var([E|Es], NewRow) :-
    (   E = '_'
    ->  NewRow = [_|NewRow1]
    ;   NewRow = [E|NewRow1]
    ),
    row_to_lgc_var(Es, NewRow1).

get_slots_for_row(Row, Slots) :-
    get_slots_concrete(Row, [], SlotsReversed),
    map(rev, SlotsReversed, Slots).

/*
 NOTE: Could use filter to filter out all lists whose length are less than
 one
 NOTE2: This is reversed, please reverse it
*/
get_slots_concrete([], A, F) :-
    (   length(A, X), X > 1
        ->  F = [A]
        ;   F = []
    ).
get_slots_concrete([E|Es], A, F) :-
    (   var(E)
        ->  A1 = [E|A],
            F = F1
        ;   E \= '#'
        ->  A1 = [E|A],
            F = F1
        ;   length(A, X), X > 1
        ->  F = [A|F1],
            A1 = []
        ;   F = F1,
            A1 = []
    ),
    get_slots_concrete(Es, A1, F1).

%% Solve functions
possible_words([], _, []).
possible_words([E|Es], Wordlist, [PossibleSolution|SlotWords]) :-
    filter(unifiable(E), Wordlist, PossibleSolution),
    possible_words(Es, Wordlist, SlotWords).

unifiable(A, B) :- \+ (B \= A).

find_min_slot([], Min, Min).
find_min_slot([Slot-Solutions|Ss], MSlot-MSol, Min) :-
    length(Solutions, SolLen),
    length(MSol, MinSolLen),
    (   SolLen < MinSolLen
        ->  find_min_slot(Ss, Slot-Solutions, Min)
        ;   find_min_slot(Ss, MSlot-MSol, Min)
    ).

try_solution(Slot, [S|_], Wordlist, NewWordlist) :-
    Slot = S,
    delete(Wordlist, S, NewWordlist).
try_solution(Slot, [_|Solutions], Wordlist, NewWordlist) :-
    try_solution(Slot, Solutions, Wordlist, NewWordlist).

has_solutions(_-[_|_]).

%% Helper functions
map(_, [], []).
map(Pred, [X|Xs], [Y|Ys]) :-
    call(Pred, X, Y),
    map(Pred, Xs, Ys).

rev(Lst, Rev) :-
    revacc(Lst, [], Rev).

revacc([], A, A).
revacc([X|Xs], A, R) :-
    revacc(Xs, [X|A], R).

filter(_, [], []).
filter(Pred, [E|Es], Flist) :-
    (   call(Pred, E)
        ->  Flist = [E|Flist1]
        ;   Flist = Flist1
    ),
    filter(Pred, Es, Flist1).

zip([], [], []).
zip([A | As], [B | Bs], [A-B | ABs]) :-
    samelength3(As, Bs, ABs),
    zip(As, Bs, ABs).

%% samelength/3
%% calculates if three lists are of the same length
samelength3([], [], []).
samelength3([_ | Xs], [_ | Ys], [_ | Zs]) :-
    samelength3(Xs, Ys, Zs).

pair_min(A1-A2, B1-B2, Min) :-
    (   A2 < B2
        ->  Min = (A1-A2)
        ;   Min = (B1-B2)
    ).

head([Head|_], Head).

not_empty([_|_]).
