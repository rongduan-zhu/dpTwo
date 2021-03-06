/*
    File:       fillin.pl
    Author:     Rongduan Zhu
    Purpose:    A program which fills in a crossword puzzle given the list of
                words.
 */

% Game Description
% We are given a crossword puzzle and the list of words which can fit into
% the puzzle. Our goal is to fill the puzzle with the list of words.

% Strategy
% The strategy I used follow the hints provided in the project specification.
% First, I replace all of the _ (underscores) in the puzzle with logical
% variables. Then I get all the slots by first getting all the slots for
% all the rows by splitting on # (hash), then I transpose the puzzle such that
% the columns become rows, I then use the same method to get the slots for
% the transposed puzzle's rows.
% (1) Then for each slot, I find the corresponding words in the word list
% such that they will fit into the slot (unifiable). I then get the slot which
% has the smallest number of unifiable words. I then (2) unify every word for
% that slot with the slot, and after unifying a word I repeat (1) as the
% possible words that will fit for that each slot would have changed. If there
% exists a slot during (1) which is not unified and has no possible solution,
% then I go back to (2) which is a choicepoint made by Prolog and try the
% next word. I stop when the word list is empty, as it implies that all the
% words have been unified with a slot.

% Loading clpfd ensures we are using the right transpose predicate
:- ensure_loaded(library(clpfd)).


%% main(PuzzleFile, WordlistFile, SolutionFile)
% The main method, reads in PuzzleFile and WordlistFile to construct a valid
% puzzle and wordlist in order for solve_puzzle to solve. It then outputs the
% filled in puzzle to SolutionFile
main(PuzzleFile, WordlistFile, SolutionFile) :-
    % Read in puzzle file
    read_file(PuzzleFile, Puzzle),
    % Read in wordlist file
    read_file(WordlistFile, Wordlist),
    % Checks to see if puzzle read in is valid
    valid_puzzle(Puzzle),
    % Solve the puzzle
    solve_puzzle(Puzzle, Wordlist, Solved),
    % Output the solved puzzle to solution file
    print_puzzle(SolutionFile, Solved).


%% read_file(Filename, Content)
% Open the file named Filename in read mode and get the stream, then use that
% stream to get each line and put in Content which is returned. Close
% the stream.
read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).


%% read_lines(Stream, Content)
% From the Stream, recursively read in every line. For each line read, check
% to see if it is end of file, if it is, check if the last line is just a
% newline line, if it is, then set that line's content to nothing, otherwise
% set Content to the content of that line. If last is not true, append
% current Line to the head of the Content so far and recursively read next
% line.
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


%% read_line(Stream, Line, Last)
% From Stream, recursively read in each character. If character is EOF, then
% set Line to [] and Last to true. Else if character is newline, then
% set Line to [] but Last to false as EOF is not reached. Else we read in a
% normal character, append that character to head of Line and recursively
% read next character.
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


%% print_puzzle(SolutionFile, Puzzle)
% Write Puzzle to SolutionFile. First open the solution as write mode. Use
% the returned Stream, apply print row to each row of puzzle.
print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).


%% print_row(Stream, Row)
% Using the provided Stream, write Row to that Stream. Do this by
% writing each character in that row to the Stream. Then write a newline.
print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).


%% put_puzzle_char(Stream, Char)
% Using the provided Stream, write Char to the Stream. If Char is a variable
% then write _, otherwise write the actual character Char is bounded to.
put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).


%% valid_puzzle
% Return true is every row is of the same length.
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(same_length(Row), Rows).


%% solve_puzzle(Puzzle, WordList, PuzzleWithVar)
% PuzzleWithVar is Puzzle with all _ replaced logical variables. WordList is
% is a list of words.
% Solves the provided Puzzle by filling in all words into puzzle. It first
% replaces all _ character with logical variables. It then splits on #
% characters to get all the slots. It then tries to find and unify all the
% words in wordlist to the slots.
solve_puzzle(Puzzle, Wordlist, PuzzleWithVar) :-
    puzzle_to_vars(Puzzle, PuzzleWithVar),
    puzzle_to_slots(PuzzleWithVar, Slots),
    solve_varpuzzle(Slots, Wordlist).


%% solve_varpuzzle(Slots, Wordlist)
% Slots are list of the slots for the puzzle and Wordlist is the list of
% available words left.
% Tries to find for a solution that fits all the words in Wordlist into all
% the slots in Slots.
% First finds for each slot, all the possible words, then it zip each slot
% with its corresponding possible words. Then the slot with the minimum
% number of possible words is found. For each of the words, it recursively
% solves the problem again but with the slot being unified to that word
% and that word taken out from Wordlist, until a solution is found.

% If there is no word left in the wordlist, it means we have successfully
% unified every word in the wordlist to a slot. We've solved the puzzle
solve_varpuzzle(_, []).

% If there are still words left in the Wordlist, we apply the strategy stated
% above.
solve_varpuzzle(Slots, Wordlist) :-
    % For each slot, get all of its possible words
    possible_words(Slots, Wordlist, PossibleSolution),
    % Zip the slot with its corresponding possible words
    zip(Slots, PossibleSolution, SlotSolution),
    % Get the head of the slot-solution to act as an initializer to find the
    % slot with the smallest number possible words
    head(SlotSolution, Head),
    % Find the slot with the smallest number of possible words
    find_min_slot(SlotSolution, Head, MinSlot-MinSlotSol),
    % For each of the words found for that slot, try all of them until one
    % solves the puzzle
    try_solution(MinSlot, MinSlotSol, Wordlist, NewWordlist),
    % Recursively do the same with the remaining words
    solve_varpuzzle(Slots, NewWordlist).


%% puzzle_to_slots(Puzzle, PuzzleWithVar)
% Puzzle is the original puzzle read from file and PuzzleWithVar has all _
% replaced with logical variables
% As the puzzle is made out of lists of strings, and each string is a row for
% the puzzle. For each row of the puzzle, we "replaces" all empty positions
% '_' with a logical variable.
puzzle_to_vars(Puzzle, PuzzleWithVar) :-
    map(row_to_logical_var, Puzzle, PuzzleWithVar).


%% puzzle_to_slots(PuzzleWithVar, Slots)
% PuzzleWithVar is the puzzle with logical variables and Slots are the slots
% retreived from PuzzleWithVar
% Convert the puzzle with all empty positions replaced with logical variables
% to slots. Does this first by finding all the slots for all the rows,
% then transpose the board, so columns becomes rows, and reapply same method.
% Then append the two slot lists together
puzzle_to_slots(PuzzleWithVar, Slots) :-
    rows_to_slots(PuzzleWithVar, RowSlots),
    transpose(PuzzleWithVar, VPuzzleWithVar),
    rows_to_slots(VPuzzleWithVar, VerticleSlots),
    append(RowSlots, VerticleSlots, Slots).


%% rows_to_slots(Rows, Slots)
% Rows are all of the rows for a puzzle, Slots are the slots retreived from
% Rows
% Converts each row to slots. For each row, find all the slots for that row,
% and combine all the slots for all rows together
rows_to_slots([], []).
rows_to_slots([Row|Rows], Slots) :-
    % Get slots for this row
    get_slots_for_row(Row, RowSlots),
    % Recursively apply the predicate above
    rows_to_slots(Rows, NewSlots),
    % Combine the slots together
    append(RowSlots, NewSlots, Slots).


%% row_to_logical_var(Chars, NewRow)
% Chars are a list of characters and NewRow is the same list of characters
% with _ replaced with logical variable
% For each character in the row (string), if the character is an _ then
% replace it with a logical variable. Otherwise keep it as the same.
row_to_logical_var([], []).
row_to_logical_var([Char|Chars], NewRow) :-
    (   Char = '_'
    ->  NewRow = [_|NewRow1]
    ;   NewRow = [Char|NewRow1]
    ),
    row_to_logical_var(Chars, NewRow1).


%% get_slots_for_row(Row, Slots)
% Row is a row of a puzzle and Slots are the slots retreived from that row
% Find all the slots in Row. Do this by splitting on #. The Slots list
% returned in first step is reversed, so we reverse the slots list and return
% the correctly ordered list of Slots
get_slots_for_row(Row, Slots) :-
    % Get all the slots by splitting on #, but the order of the slots are
    % reversed
    get_slots_for_row(Row, [], ReversedSlots),
    % Reverse the reversed slots list
    reverse(ReversedSlots, Slots).


%% get_slots_for_row(Row, Slot, Slots)
% Row is a row of a puzzle, Slot is a new slot constructed from
% reading the row, and Slots are the slots constructed so far from this row
% Find all the slots for Row. Do this by first traversing the row, stop until
% a character is found, append slot constructed so far to Slots, and
% recursively apply the same strategy until there is no characters in the row
% left. Basically splitting the row on # characters.
get_slots_for_row(Chars, SlotsAcc, Slots) :-
    (   Chars \= []
        ->  % As long as there is still characters, get all characters until
            % hitting a # character.
            get_slot(Chars, Slot, CharsLeft),
            % Append the list of characters not including the # to
            % the list of slots.
            append_slot(Slot, SlotsAcc, NewSlots),
            % Recursively apply the same strategy.
            get_slots_for_row(CharsLeft, NewSlots, Slots)
        ;   Slots = SlotsAcc
    ).


%% get_slot(Chars, Slot, CharsLeft)
% Chars are the characters of a row, and Slot is a new slot constructed so
% far from the row, and CharsLeft is the characters left after trying to
% construct a new slot
% Return all characters from Chars from left to right until a # character is
% found. The returned list of characters does not contain the # character.
% If the character is not bound or if the character is bound but not bounded
% to #, then append that character to the tail of list. Otherwise stop and
% return the list of characters found so far.
get_slot([Char|Chars], Slot, CharsLeft) :-
    (   var(Char)
        ->  Slot = [Char|Tail],
            get_slot(Chars, Tail, CharsLeft)
        ;   Char \= '#'
        ->  Slot = [Char|Tail],
            get_slot(Chars, Tail, CharsLeft)
        ;   Slot = [],
            CharsLeft = Chars
    ).
get_slot([], [], []).


%% append_slot(Slot, Slots, NewSlots)
% Slot is a new Slot, Slots is slots found so far, and NewSlots is the result
% of appending Slot to Slots
% Only append Slot to Slots if the length of Slot is greater than 1. Doing
% this because a Slot with 1 character or less is not a valid slot according
% to project specification
append_slot(Slot, Slots, NewSlots) :-
    (   length(Slot, Len), Len > 1
        ->  NewSlots = [Slot|Slots]
        ;   NewSlots = Slots
    ).

%% possible_words(Slots, Wordlist, PossibleWords)
% Slots is all of the unbound slots for this puzzle, Wordlist is all of
% the words that is not bounded to a slot and PossibleWords is a list
% of list of words, each list of words correspond to a slot
% For each slot, find the corresponding words in Wordlist that can be unified
% with the slot.
possible_words([], _, []).
possible_words([Slot|Slots], Wordlist, [PossibleSolution|SlotWords]) :-
    filter(unifiable(Slot), Wordlist, PossibleSolution),
    possible_words(Slots, Wordlist, SlotWords).


%% unifiable(A, B)
% Returns true if A can be unified with B or vice versa
unifiable(A, B) :- \+ (A \= B).


%% find_min_slot(SlotSolutionPair, CurrentMinPair, MinPair)
% SlotSolutionPair is all of the Slot Solution pair, CurrentMinPair is the
% Slot with the smallest wordlist found so far, and MinPair is the slot
% with smallest wordlist for all of the slots.
% Finds the Slot-Solution pair which has the smallest number of words in its
% Solution
find_min_slot([], Min, Min).
find_min_slot([Slot-Solutions|Ss], MSlot-MSol, Min) :-
    % Find the number of words in current Solution
    length(Solutions, SolLen),
    % Find the number of words in smallest Solution
    length(MSol, MinSolLen),
    (   SolLen =:= 0
        ->  % If the length is 0, then the slot must be unified already, if
            % not then backtrack
            is_unified(Slot),
            find_min_slot(Ss, MSlot-MSol, Min)
        ;   SolLen =:= 1
        ->  % If the length is 1 then we can stop searching for min pair
            % because this slot will be one of the best slot we can try
            find_min_slot([], Slot-Solutions, Min)
        ;   SolLen < MinSolLen
        ->  % If current solution length is less than current me, set current
            % min pair to current solution and its corresponding slot
            find_min_slot(Ss, Slot-Solutions, Min)
        ;   % Current min is still the minimum, continue with rest of slots
            find_min_slot(Ss, MSlot-MSol, Min)
    ).


%% try_solution(Slot, Solutions, Wordlist, NewWordlist)
% Slot is a slot for a puzzle, Solutions are all the words that can be
% unified with the slot, Wordlist is the words left that is not bounded to
% a slot, and NewWordlist with a word deleted, where word is the word that
% got bounded to the Slot.
% For Slot, try all of its solutions. One of them must result in a solution
% assuming valid wordlist and puzzle.

% Unify word with slot, and return the new wordlist without that word.
try_solution(Slot, [Solution|_], Wordlist, NewWordlist) :-
    Slot = Solution,
    delete(Wordlist, Solution, NewWordlist).
% The previous word must've failed which resulted in Prolog backtrack to this
% choicepoint. So lets try the next solution
try_solution(Slot, [_|Solutions], Wordlist, NewWordlist) :-
    try_solution(Slot, Solutions, Wordlist, NewWordlist).


%% is_unified(Elements)
% Elements is a slot of the puzzle
% See if all of the elements is bound, i.e. it is not a variable
is_unified([]).
is_unified([Element|Elements]) :-
    nonvar(Element),
    is_unified(Elements).


%% map(Predicate, List, NewList)
% Apply predicate to every element in list. This does not work in all
% modes as it is not required for this project.
map(_, [], []).
map(Pred, [X|Xs], [Y|Ys]) :-
    call(Pred, X, Y),
    map(Pred, Xs, Ys).


%% filter(Predicate, List, FilteredList)
% Filter out all elements in list that does not satisfy when predicate is
% applied to that element. This does not work in all modes as it is not
% required for this project.
filter(_, [], []).
filter(Pred, [X|Xs], Filtered) :-
    (   call(Pred, X)
        ->  Filtered = [X|Filtered1]
        ;   Filtered = Filtered1
    ),
    filter(Pred, Xs, Filtered1).


%% zip(List1, List2, Zipped)
% Zip all elements in list1 with list2. This does not work in all modes as it
% is not required for this subject.
zip([], [], []).
zip([X | Xs], [Y | Ys], [X-Y | XYs]) :-
    zip(Xs, Ys, XYs).


%% head(List, Head)
% Get the head of list.
head([Head|_], Head).
