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

count([], (0,0)).
count([H|T], (P1, P2)) :- 
	count_row(H, (Pn, Ps)), count(T, (S1, S2)), P1 is Pn + S1, P2 is Ps + S2.

count_row([], (0,0)).
count_row([.|T], (P1, P2)) :- count_row(T, (P1,P2)).
count_row([1|T], (P1, P2)) :- count_row(T, (Pn, P2)), P1 is Pn + 1.
count_row([2|T], (P1, P2)) :- count_row(T, (P1, Pn)), P2 is Pn + 1.
