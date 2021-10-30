:- use_module(library(clpfd)).

% Source: https://www.metalevel.at/sudoku/sudoku.pl

sudoku(Rows) :-
        length(Rows, 9),                  % Sudokus have 9 rows
        maplist(same_length(Rows), Rows), % that are the same length
        append(Rows, Vs), Vs ins 0..8,    % and hold digits 0 to 8
        maplist(all_distinct, Rows),      % that are distinct,
        transpose(Rows, Columns),         % whose columns
        maplist(all_distinct, Columns),   % also hold distinct values,
        Rows = [As, Bs, Cs,               % and if
                Ds, Es, Fs,               % we take
                Gs, Hs, Is],              % all the rows,
        blocks(As, Bs, Cs),               % split them in triples,
        blocks(Ds, Es, Fs),               % and for each triple
        blocks(Gs, Hs, Is).               % what we do
blocks([], [], []).                       % until we reach the end
blocks([N1,N2,N3|Ns1],                    % is to
       [N4,N5,N6|Ns2],                    % take
       [N7,N8,N9|Ns3]) :-                 % three columns
       all_distinct([N1,N2,N3,            % so that
                     N4,N5,N6,            % the resulting 3x3 squares
                     N7,N8,N9]),          % also hold distinct values,
        blocks(Ns1, Ns2, Ns3).            % all of them.

ucn(UCN) :-                        % Let's get a Sudoku-compatible
                                   % 9-free Unique Citiznship Number.
    UCN = [Y1,Y2,                  % It holds a year,
           M1,M2,                  % a month,
           D1,D2,                  % a day,
           X1,X2,                  % some other digits
           X3,X4],                 % and some more digits.
    Y1 = 6,                        % Agents are born in the 60s,
    Y2 in 0..8,                    % any year before the moon landing
    M1 in 0..1,                    % any month
    M2 in 0..8,                    % except "wake me up when it ends"
    M1 * 10 + M2 #=< 12,           % up until December
    D1 in 0..3,                    % any day
    D2 in 0..8,                    % except 9 and 19
    D1 * 10 + D2 #=< 31,           % up until 31,
                                   % not caring about date validity,
                                   % as the UCN will be handpicked.
    [X1,X2,X3] ins 0..8,           % Next three digits are not 9,
    X4 = 5,                        % the last will be broadcasted 5.
    append(SRow,[X4],UCN),         % If we remove the last digit
    all_distinct(SRow),            % all digits should be distinct.
    label(UCN),                    % Generate a UCN
    ucn_weights(Ws),               % that is valid, i.e.,
    scalar_product(SRow,Ws,#=,C),  % the checksum of the first digits
    X4 #= C mod 11,                % should match the last one,
    portray_clause(UCN).           % and print it.

ucn_weights([2,4,8,5,10,9,7,3,6]). % magic UCN checksum weights

sudokuFirstRow(Row, Rows) :-       % We want
    sudoku(Rows),                  % a Sudoku
    Rows = [Row|_],                % with a given first row
    maplist(label, Rows),          % generated
    maplist(portray_clause, Rows). % and printed.

% forget an element at an index I in a list
forget([], [], _).
forget([_|L1],[_|L2],0) :- !, forget(L1, L2, -1).
forget([X|L1],[X|L2],I) :- I1 is I - 1, forget(L1, L2, I1).

% forget an element at an index J in a list at an index I
forgetOne([], [], _, _).
forgetOne([X1|L1],[X2|L2],0,J) :- !, forget(X1,X2,J),
                                     forgetOne(L1,L2,-1,J).
forgetOne([X|L1],[X|L2],I,J)   :- I1 is I - 1, forgetOne(L1,L2,I1,J).

% count Sudoku solutions (0, 1, or 2)
countSolutions(Sudoku, N) :-
    findnsols(2, Sudoku,                % generate at most 2
              (sudoku(Sudoku),          % Sudoku
               maplist(label, Sudoku)), % solutions
              Solutions),               % in a list
    !, length(Solutions, N).            % that must be of length N

% generate a puzzle from a solved Sudoku by deleting random elements
generateSudokuPuzzle(Attempt, Puzzle, MaxTries) :-
    % starting the count from 0
    generateSudokuPuzzle(Attempt, Puzzle, 0, MaxTries).

generateSudokuPuzzle(Attempt, Attempt, Tries, MaxTries) :-
    Tries > MaxTries, !,               % On maximum iterations reached
    maplist(portray_clause, Attempt).  % display the puzzle.


generateSudokuPuzzle(Attempt, Puzzle, Tries, MaxTries) :-
    random_between(0, 8, I),                % Generate random row
    random_between(0, 8, J),                % and random column
    forgetOne(Attempt, NewOne, I, J),       % forget the number there,
    countSolutions(NewAttempt, 1), !,       % still unique solution,
    Tries1 is Tries + 1,                    % increment tries
    generateSudokuPuzzle(NewOne, Puzzle,    % and
                         Tries1, MaxTries). % carry on.

% Oops, number of solutions maybe more than one!
generateSudokuPuzzle(Attempt, Puzzle, Tries, MaxTries) :-
    Tries1 is Tries + 1,                    % Increment tries,
    generateSudokuPuzzle(Attempt, Puzzle,   % keep calm
                         Tries1, MaxTries). % and carry on.

% put everything together
generateUCNAndPuzzle(UCN, Puzzle, SolvedPuzzle) :-
    ucn(UCN),                          % Generate a UCN,
    append(Row,[_],UCN),               % trim last digit for a row,
    sudokuFirstRow(Row, SolvedPuzzle), % generate solved Sudoku
    generateSudokuPuzzle(SolvedPuzzle, % and then
                         Puzzle, 100). % generate Sudoku puzzle.
