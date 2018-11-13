pvars([x,y,z]).
pvar(X):-pvars(V),member(X,V).

power(X):-pvar(X),!.
power(X^Y):-pvar(X),integer(Y),Y>1,!.
power(-X^Y):-pvar(X),integer(Y),Y>1,!.
coefficient(K):-number(K).

monomial(X):-pvar(X),!.
monomial(-X):-pvar(X),!.
monomial(N):-number(N),!.
monomial(X):-power(X),!.
monomial(K*X):-coefficient(K),power(X),!.

polynomial(M):-monomial(M),!.
polynomial(P+M):-monomial(M),polynomial(P),!.
polynomial(P-M):-monomial(M),polynomial(P),!.

poly2list(P+M, [M|T]):-monomial(M), poly2list(P, T), polynomial(P),!.
poly2list(P-M, [-M|T]):-monomial(M), poly2list(P, T),  polynomial(P),!.
% poly2list(P-M, [P|T]):-monomial(P), polynomial(M),! , poly2list(M, T).
poly2list(M, [M]):-monomial(M),!.
poly2list(-M, [-M]):-monomial(M),!.
% poly2list(-P, [-P]):-monomial(P),!.
