pvars([x,y,z]).
pvar(X):-pvars(V),member(X,V).

power(X):-pvar(X),!.
power(X^Y):-pvar(X),integer(Y),Y>1,!.
coefficient(K):-number(K).

monomial(X):-pvar(X),!.
monomial(N):-number(N),!.
monomial(X):-power(X),!.
monomial(-K*X):-coefficient(K), power(X), !.
monomial(K*X):-coefficient(K),power(X),!.

polynomial(M):-monomial(M).
polynomial(-M):- monomial(-M).
polynomial(M+P):- monomial(M),polynomial(P),!.
polynomial(M-P):- monomial(M),polynomial(-P),!.
polynomial(-M+P) :- monomial(-M), polynomial(P), !.

%% 2*x^2-3*y^2 -> [2*x^2, -3*y^2] 
poly2list(-M+P, Res) :- poly2list(-M, Res1), poly2list(P, Res2), append(Res1, Res2, Res).
poly2list(M-P, Res) :- poly2list(M, Res1), poly2list(-P, Res2), append(Res1, Res2, Res).
poly2list(M+P, Res) :- poly2list(M, Res1), poly2list(P, Res2), append(Res1, Res2, Res).
poly2list(-M, [-M]) :- monomial(M), !. 
poly2list(M, [M]) :- monomial(M), !. 