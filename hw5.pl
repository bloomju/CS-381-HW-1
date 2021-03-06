% CS381, Spring 2018
% Assignment 5
% Justin Bloom, Meghana Kolasani, Rushil Vora

% Exercise 1:

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

% Part (a)
schedule(N, P, T) :- enroll(N, X), where(X, P), when(X, T).
% Part (b)
usage(P, T) :- where(X, P), when(X,T).
% Part (c)
conflict(A, B) :- where(A,P), where(B,P), when(A,T), when(B,T), A \= B.
% Part (d)
meet(S1, S2) :- schedule(S1, P, T), schedule(S2, P, T), S1\=S2;
	        schedule(S1, P, T1), schedule(S2, P, T2), S1\=S2, T2 =:= T1+1.

% Exercise 2:

% Part (a)
rdup(L, M) :- cdup(L, M).
cdup([], []).
cdup([N|X], [N|Y]) :- cdup(X, Y), not(member(N, X)).
cdup([N|X], Y) :- cdup(X, Y), member(N, X).

% Part (b)
flat(L, F) :- flat(L, [], F).
flat([], F, F).
flat([C|D], L, F) :- flat(C, L1, F), flat(F, L, L1).
flat(C, F, [C|F]) :- not(is_list(C)).

% Part (c)

project(_,[],[]).
project([],_,[]).
project(1,[X|_], X) :- !.
project(N,[_|L] X) :- not(is_list(N)), N1 is N-1, project(N1, L, X).
project([N1|N], L, [X1|XR]) :- project(N1, L, X1), project(N, L, XR).
projectDriver(A, B, M) :- project(A, B, L), flat(L, M).
