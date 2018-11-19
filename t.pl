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

%%simplification of monnomials (receives a single monnomial to simplify)

simmon(1*M, M) :- power(M), !.
simmon(0*_, 0) :- !.
simmon(M, M).

%% decompose a monomial in coefficient and variable-exponent

monparts(X^N, 1, X^N) :- power(X^N), !. %% case with no coefficient
monparts(K*M, K, M) :- number(K), !. 
monparts(K, K, novarexp) :- number(K), !. %% case with only number
monparts(X, 1, X) :- pvar(X), !. %% case with var only

%% deletes the monomial with variable-exponent Exp yielding polynomial P2

delmonomial([],Exp,0*Exp,[]).
delmonomial([M|L], Exp, M2, Lr):- monomial(M), monparts(M,_,Exp), delmonomial(L,Exp,MX,Lr), addmonomial(M,MX,M2).
delmonomial([M|L], Exp, M2, [M|Lr]):- monomial(M), monparts(M,_,Exp2),not(Exp=Exp2), delmonomial(L,Exp,M2,Lr).

%% creates the monnomial that is the sum of both, K is coefficient, Exp is the variable-exponent

aux_addmonomial(K,novarexp,K):-!.
aux_addmonomial(0,_,0):-!.
aux_addmonomial(1,Exp,Exp):-!.
aux_addmonomial(K,Exp,K*Exp).  

%% adds two monnomials together (returns false if not able to sum)
addmonomial(K1,K2,K3):- number(K1),number(K2),!,						%%only coefficients
K3 is K1+K2.
addmonomial(M1,M2,Res):- monparts(M1,K1,Exp), monparts(M2,K2,Exp),		%% two monnomials
K3 is K1+K2, aux_addmonomial(K3,Exp,Res).


%%simplification of polynomes in expression format

simpoly(P, ResList) :- polynomial(P), poly2list(P, L), simpoly_list(L, ResList), poly2list(Res, ResList).

%% simplification of polynomes in list format
simpoly_list([],[]).
simpoly_list([P|0], [P]) :- !.	
simpoly_list([0,M], [M]) :- monomial(M), !.
simpoly_list([M|P], [M2|PF]) :- monparts(M, _, Exp), delmonomial([M|P], Exp, M2, P2), simpoly_list(P2, PF).
simpoly_list([M|P], [M1|P1]) :- simpoly_list(P, P1), simmon(M, M1) .


poly2list(P - M, [Neg*Exp|T]):-monomial(M), poly2list(P, T),  polynomial(P),monparts(M, C, Exp), Neg is C*(-1),!.
poly2list(P + M, [M|T]):-monomial(M), poly2list(P, T), polynomial(P),!.
poly2list(-M, [Neg*Exp]):-monomial(M),monparts(M, C, Exp), Neg is C*(-1),!.
poly2list(M, [M]):-monomial(M),!.


poly2list2([], 0).
poly2list2([M|T], M+R):-poly2list2(T, R).

to_list(L, R):-poly2list2(L, A), format(chars(R), "~w", A).

% correct([A1|T], [A1|Res]):-A1==')', correct([A2,A3|T], Res).
% correct([], []).
% correct([X|T], [Res]):-X=='(',correct(T, Res).
% correct([X|T], [X|Res]):-correct(T, Res).
% correct([A1,A2,A3|T], [-|Res]):-A1=='+',A2=='(',A3=='-', correct(T, Res).
correct(L, R):-delete(L, '(', S), delete(S, ')', R).