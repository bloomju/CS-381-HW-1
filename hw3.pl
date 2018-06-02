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
