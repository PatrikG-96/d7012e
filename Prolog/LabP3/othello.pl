/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Patrik Guthenberg
%    Student user id  : ?
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    [.,.,1,2,.,.], 
	    [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    [.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(InitialState, InitialPlyr) :- initBoard(InitialState), InitialPlyr = 1.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

winner(State, Plyr) :- 
	terminal(State),
	count(State, R),
	win(R, Plyr).

win((X,Y), 1) :- X > Y.
win((X,Y), 2) :- X < Y.

count([], (0,0)).
count([H|T], (P1, P2)) :- 
	count_row(H, (Pn, Ps)), count(T, (S1, S2)), P1 is Pn + S1, P2 is Ps + S2.

count_row([], (0,0)).
count_row([.|T], (P1, P2)) :- count_row(T, (P1,P2)).
count_row([1|T], (P1, P2)) :- count_row(T, (Pn, P2)), P1 is Pn + 1.
count_row([2|T], (P1, P2)) :- count_row(T, (P1, Pn)), P2 is Pn + 1.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :- 
	terminal(State),
	count(State, (P1, P2)),
	P1 = P2.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   


terminal(State) :- 
	moves(1, State, M1),
	moves(2, State, M2),
	M1 = [n],
	M2 = [n].


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%


moves(Plyr, State, MvList) :- 
	Other is mod(Plyr*2,3),
	setof([X,Y], check_move(Plyr, Other, State, [X,Y]), R), quicksort(R, MvList).
 
 moves(Plyr, State, [n]) :-
	Other is mod(Plyr*2,3),
	findall([X,Y], check_move(Plyr, Other, State, [X,Y]), R),
	length(R, L), L = 0.

quicksort([],[]) :- !.
quicksort([P|T],Sorted) :-
    partition(P,T, Left,Right),
    quicksort(Left,Ls),
    quicksort(Right,Rs),
    conc(Ls,[P|Rs],Sorted).


partition(_, [], [], []) :- !.
partition(P, [E|T], Left, [E|Right]) :- 
    bigger(E, P), partition(P, T, Left, Right).
partition(P, [E|T], [E|Left], Right)  :- 
    bigger(P, E), partition(P, T, Left, Right).


bigger([X1,Y1], [X1,Y1]) :- fail.
bigger([X1,Y1], [X1,Y2]) :-
    Y1 > Y2.
bigger([X1,_], [X2,_]) :-
    X1 > X2.

conc( [], L, L).
conc( [X| L1], L2, [X| L3]) :- conc( L1, L2, L3).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

nextState(1, Move, State, NewState, NextPlyr) :- 
	make_move(State, 1, 2, Move, NewState), 
	NextPlyr = 2.
nextState(2, Move, State, NewState, NextPlyr) :-
	make_move(State, 2, 1, Move, NewState),
	NextPlyr = 1.
nextState(1, n, State, State, 2).
nextState(2, n, State, State, 1).

make_move(State, Plyr, Other, [X,Y], NewState) :-
	%write("Player "), write(Plyr), write(" making a move at "), write(Move),
	X1 is X+1, Y1 is Y+1, X2 is X-1, Y2 is Y-1,
	set(State, NextState, [X,Y], Plyr),
	flipRight(NextState, Plyr, Other, [X1,Y], S1), flipLeft(S1, Plyr, Other, [X2,Y], S2), 
	flipNorth(S2, Plyr, Other, [X,Y2], S3), flipSouth(S3, Plyr, Other, [X,Y1], S4),
	flipNE(S4, Plyr, Other, [X1,Y2], S5), flipNW(S5, Plyr, Other, [X2,Y2], S6), 
	flipSE(S6, Plyr, Other, [X1,Y1], S7), flipSW(S7, Plyr, Other, [X2,Y1], NewState).




flipRight(State, Plyr, Other, [X,Y], NewState):-
	in_bounds(X,Y), in_slot(Other, State, [X,Y]), set(State, S1, [X,Y], Plyr), X2 is X+1, flipRight(S1, Plyr, Other, [X2,Y], NewState).
flipRight(State, Plyr, _, [X,Y], State) :-
	in_bounds(X,Y), in_slot(Plyr, State, [X,Y]).
flipRight(State, _, _, _, State).

flipLeft(State, Plyr, Other, [X,Y], NewState) :- 
	in_bounds(X,Y), in_slot(Other, State, [X,Y]), set(State, S1, [X,Y], Plyr), X2 is X-1, flipLeft(S1, Plyr, Other, [X2,Y], NewState).
flipLeft(State, Plyr, _, [X,Y], State) :- 
	in_bounds(X,Y), in_slot(Plyr, State, [X,Y]).
flipLeft(State, _, _, _, State).

flipNorth(State, Plyr, Other, [X,Y], NewState) :- 
	in_bounds(X,Y), in_slot(Other, State, [X,Y]), set(State, S1, [X,Y], Plyr), Y2 is Y-1, flipNorth(S1, Plyr, Other, [X,Y2], NewState).
flipNorth(State, Plyr, _, [X,Y], State) :- 
	in_bounds(X,Y), in_slot(Plyr, State, [X,Y]).
flipNorth(State, _, _, _, State).

flipSouth(State, Plyr, Other, [X,Y], NewState) :- 
	in_bounds(X,Y), in_slot(Other, State, [X,Y]), set(State, S1, [X,Y], Plyr), Y2 is Y+1, flipSouth(S1, Plyr, Other, [X,Y2], NewState).
flipSouth(State, Plyr, _, [X,Y], State) :- 
	in_bounds(X,Y), in_slot(Plyr, State, [X,Y]).
flipSouth(State, _, _, _, State).

flipNE(State, Plyr, Other, [X,Y], NewState) :- 
	in_bounds(X,Y), in_slot(Other, State, [X,Y]), set(State, S1, [X,Y], Plyr), X2 is X+1, Y2 is Y-1, flipNE(S1, Plyr, Other, [X2,Y2], NewState).
flipNE(State, Plyr, _, [X,Y], State) :- 
	in_bounds(X,Y), in_slot(Plyr, State, [X,Y]).
flipNE(State, _, _, _, State).

flipNW(State, Plyr, Other, [X,Y], NewState) :- 
	in_bounds(X,Y), in_slot(Other, State, [X,Y]), set(State, S1, [X,Y], Plyr), X2 is X-1, Y2 is Y-1, flipNW(S1, Plyr, Other, [X2,Y2], NewState).
flipNW(State, Plyr, _, [X,Y], State) :- 
	in_bounds(X,Y), in_slot(Plyr, State, [X,Y]).
flipNW(State, _, _, _, State).


flipSE(State, Plyr, Other, [X,Y], NewState) :- 
	in_bounds(X,Y), in_slot(Other, State, [X,Y]), set(State, S1, [X,Y], Plyr), X2 is X+1, Y2 is Y+1, flipSE(S1, Plyr, Other, [X2,Y2], NewState).
flipSE(State, Plyr, _, [X,Y], State) :- 
	in_bounds(X,Y), in_slot(Plyr, State, [X,Y]).
flipSE(State, _, _, _, State).

flipSW(State, Plyr, Other, [X,Y], NewState) :- 
	in_bounds(X,Y), in_slot(Other, State, [X,Y]), set(State, S1, [X,Y], Plyr), X2 is X-1, X2 =< 5, Y2 is Y+1, Y2 >= 0,  flipSW(S1, Plyr, Other, [X2,Y2], NewState).
flipSW(State, Plyr, _, [X,Y], State) :- 
	in_bounds(X,Y), in_slot(Plyr, State, [X,Y]).
flipSW(State, _, _, _, State).


in_bounds(X,Y) :- 
	X > 0, X < 6, Y > 0, Y < 6.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

validmove(Plyr, State, Proposed) :- 
	moves(Plyr, State, R), 
	member(Proposed, R).

is_empty(State, [X,Y]) :- get(State, [X,Y], R), R = '.'.  
in_slot(Plyr, State, [X,Y]) :- get(State, [X,Y], R), R = Plyr.

% East
check_move(Plyr, Other, State, [X,Y]) :- 
	is_empty(State, [X,Y]), X2 is X+1, X2 < 5, 
	in_slot(Other, State, [X2,Y]), 
	valid_east(State, Plyr, Other, [X2, Y]).
% West
check_move(Plyr, Other, State, [X,Y]) :- 
	is_empty(State, [X,Y]), X2 is X-1, X2 > 0,
	in_slot(Other, State, [X2,Y]), valid_west(State, Plyr, Other, [X2, Y]).
% North
check_move(Plyr, Other, State, [X,Y]) :- 
	is_empty(State, [X,Y]), Y2 is Y-1, Y2 > 0,
	in_slot(Other, State, [X,Y2]), valid_north(State, Plyr, Other, [X, Y2]).
% South
check_move(Plyr, Other, State, [X,Y]) :- 
	is_empty(State, [X,Y]), Y2 is Y+1, Y2 < 5,
	in_slot(Other, State, [X,Y2]), valid_south(State, Plyr, Other, [X, Y2]).
% NE
check_move(Plyr, Other, State, [X,Y]) :- 
	is_empty(State, [X,Y]), Y2 is Y-1, Y > 0, X2 is X+1, X2 < 5,
	in_slot(Other, State, [X2,Y2]), valid_NE(State, Plyr, Other, [X2, Y2]).
% NW
check_move(Plyr, Other, State, [X,Y]) :- 
	is_empty(State, [X,Y]), Y2 is Y-1, Y > 0, X2 is X-1, X2 > 0,
	in_slot(Other, State, [X2,Y2]), valid_NW(State, Plyr, Other, [X2, Y2]).
% SE
check_move(Plyr, Other, State, [X,Y]) :- 
	is_empty(State, [X,Y]), Y2 is Y+1, Y > 0, X2 is X+1, X2 < 5,
	in_slot(Other, State, [X2,Y2]), valid_SE(State, Plyr, Other, [X2, Y2]).
% SW
check_move(Plyr, Other, State, [X,Y]) :- 
	is_empty(State, [X,Y]), Y2 is Y+1, Y > 0, X2 is X-1, X2 > 0,
	in_slot(Other, State, [X2,Y2]), valid_SW(State, Plyr, Other, [X2, Y2]).

% From check_move, it's established that the move has the other players stone to the east of it.
% Therefore, we keep searching east until we find either "." or current players stone. "." means
% the move is invalid, never finding current players stone also means this, finding current players
% stone means the move is valid
valid_east(State, Plyr, _, [X,Y]) :- X =< 5, in_slot(Plyr, State, [X,Y]).
valid_east(State, Plyr, Other, [X,Y]) :- X < 5, in_slot(Other, State, [X,Y]), X2 is X+1,
								  		 X2 is X+1, valid_east(State, Plyr, Other, [X2, Y]).

valid_west(State, Plyr, _, [X,Y]) :- X >= 0, in_slot(Plyr, State, [X,Y]).
valid_west(State, Plyr, Other, [X,Y]) :- X > 0, in_slot(Other, State, [X,Y]), X2 is X-1,
								  		 valid_west(State, Plyr, Other, [X2, Y]).

valid_north(State, Plyr, _, [X,Y]) :- Y >= 0, in_slot(Plyr, State, [X,Y]).
valid_north(State, Plyr, Other, [X,Y]) :- Y > 0, in_slot(Other, State, [X,Y]), Y2 is Y-1,
								  		 valid_north(State, Plyr, Other,  [X, Y2]).

valid_south(State, Plyr, _, [X,Y]) :- Y =< 5, in_slot(Plyr, State, [X,Y]).
valid_south(State, Plyr, Other, [X,Y]) :- Y < 5, in_slot(Other, State, [X,Y]), Y2 is Y+1,
								  		 valid_south(State, Plyr, Other,  [X, Y2]).

valid_NE(State, Plyr, _, [X,Y]) :- X =< 5, Y >= 0, in_slot(Plyr, State, [X,Y]).
valid_NE(State, Plyr, Other, [X,Y]) :- X < 5, Y > 0, in_slot(Other, State, [X,Y]), X2 is X+1, Y2 is Y-1,
								  		 valid_NE(State, Plyr, Other,  [X2, Y2]).

valid_NW(State, Plyr, _, [X,Y]) :- X >= 0, Y >= 0, in_slot(Plyr, State, [X,Y]).
valid_NW(State, Plyr, Other, [X,Y]) :- X > 0, Y > 0, in_slot(Other, State, [X,Y]), X2 is X-1, Y2 is Y-1,
								  		 valid_NW(State, Plyr, Other,  [X2, Y2]).

valid_SE(State, Plyr, _, [X,Y]) :- X =< 5, Y =< 5, in_slot(Plyr, State, [X,Y]).
valid_SE(State, Plyr, Other, [X,Y]) :- X < 5, Y < 5, in_slot(Other, State, [X,Y]), X2 is X+1, Y2 is Y+1,
								  		 valid_SE(State, Plyr, Other,  [X2, Y2]).

valid_SW(State, Plyr, _, [X,Y]) :- X >= 0, Y =< 5, in_slot(Plyr, State, [X,Y]).
valid_SW(State, Plyr, Other, [X,Y]) :- X > 0, Y < 5, in_slot(Other, State, [X,Y]), X2 is X-1, Y2 is Y+1,
								  		 valid_SW(State, Plyr, Other,  [X2, Y2]).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(State,1) :- winner(State,1), !.
h(State,-1) :- winner(State,2), !.
h(State,0) :- tie(State), !.
h(_,0). % otherwise no heuristic guidance used



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.


lowerBound(-37).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.


upperBound(37).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
