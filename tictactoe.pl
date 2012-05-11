% Players
player(Turn, Sym):-
    P is Turn mod 2,
    (P = 1 -> Sym = x; Sym = o).
other(P1, P2):-
    P1 = x -> P2 = o; P2 = x.

% Winning states
horizontal(X, [X, X, X | _]):- !.
horizontal(X, [_, _, _ | Board]):- horizontal(X, Board).

vertical(X, [X, _, _ | Board]):- vertical(X, Board).
vertical(X, [X, _]).
vertical(X, [X]).
vertical(_, []).

diagonal(X, [X, _, _, _, X, _, _, _, X]).
diagonal(X, [_, _, X, _, X, _, X, _, _]).

win(X, Board):- horizontal(X, Board), !.
win(X, Board):- diagonal(X, Board), !.
win(X, [A, B, C | Board]):- vertical(X, [A, B, C | Board]), !.
win(X, [_, B, C | Board]):- vertical(X, [B, C | Board]), !.
win(X, [_, _, C | Board]):- vertical(X, [C | Board]), !.

% Board generation and manipulation

set(X, [e|Board], [X|Board]).
set(X, [P|Board], [P|NBoard]):-
    set(X, Board, NBoard).
set_nth1(1, X, [_|T], [X|T]):- !.
set_nth1(N, X, [H|T], [H|R]):-
    N1 is N - 1, set_nth1(N1, X, T, R).

next_move(P, Board, NBoard):-
    set(P, Board, NBoard).

% Minmax
myplay(state(_, 1, _), [x,e,e, e,e,e, e,e,e], 0):- !.
myplay(state(Me, Turn, Board), NBoard, IWon):-
    print('Next Move Scores: '),
    aggregate_all(max(V, Board1),
                  (next_move(Me, Board, Board1),
                   minmax(Me, Turn, Board1, V), print(V), print(', ')),
                  max(V, NBoard)),
    nl,
    (V = 1, win(Me, NBoard) -> IWon = 1; IWon = 0).

minmax(P, _, Board, 1):-
    win(P, Board), !.
minmax(_, 9, _, 0):- !.
minmax(P, Turn, Board, V):-
    other(P, P1), Turn1 is Turn+1,
    aggregate_all(min(V1),
                  (next_move(P1, Board, Board1),
                   maxmin(P1, Turn1, Board1, V1)),
                  V).

maxmin(P, _, Board, -1):-
    win(P, Board), !.
maxmin(_, 9, _, 0):- !.
maxmin(P, Turn, Board, V):-
    other(P, P1), Turn1 is Turn+1,
    aggregate_all(max(V1),
                  (next_move(P1, Board, Board1),
                   minmax(P1, Turn1, Board1, V1)),
                  V).

% I/O and user interaction
portray(state(_, Turn, [A,B,C, D,E,F, G,H,I])):-
    Turn1 is Turn - 1,
    writef("Turn: %t\n%t %t %t\n%t %t %t\n%t %t %t\n", 
           [Turn1, A,B,C, D,E,F, G,H,I]).
portray(e):-
    write('.').

start:-
    start(2).
start(Player):-
    Player > 0, Player < 3,
    retractall(gameover),
    MyP is Player + 1,
    player(MyP, Me),
    S = state(Me, 1, [e,e,e, e,e,e, e,e,e]),
    (Player = 2 -> myplay(S, NBoard, _), S2 = state(Me, 2, NBoard);
                   nb_setval(state, S), S2 = S),
    print(S2),
    nb_setval(state, S2),
    writef("%s\n", ["\nUse play(Row, Col) to make moves! Row and Col are 1-indexed"]).

play(_, _):-
    gameover, !,
    write('I already won :| start a new game.').
play(Row, Col):-
    Pos is (Row-1) * 3 + Col,
    nb_getval(state, state(Me, Turn, Board)),
    nth1(Pos, Board, e), !,
    other(Me, Hir),
    set_nth1(Pos, Hir, Board, Board1), 
    Turn1 is Turn+1,
    S1 = state(Me, Turn1, Board1),
    print(S1),
    myplay(S1, NBoard, IWon),
    (IWon = 1 -> assert(gameover); true),
    Turn2 is Turn1 + 1,
    S2 = state(Me, Turn2, NBoard),
    print(S2),
    nb_setval(state, S2).
play(_, _):-
    write('That move is illegal.'), fail.
