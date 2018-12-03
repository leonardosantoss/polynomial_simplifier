digits(0)-->[zero].
digits(1)-->[one].
digits(2)-->[two].
digits(3)-->[three].
digits(4)-->[four].
digits(5)-->[five].
digits(6)-->[six].
digits(7)-->[seven].
digits(8)-->[eight].
digits(9)-->[nine].
digits(10)-->[ten].
digits(11)-->[eleven].
digits(12)-->[twelve].

% digits(R)-->[fifty],digits(X),write(R), R is 50 + X.

% aux(A,B,C):-A is B + C.

pvar(x)-->[x].
pvar(y)-->[y].
pvar(z)-->[z].

times-->[times].
raised-->[raised,to].

power(N)-->digits(N).

coef(N)-->digits(N).

exp(P^PO)-->pvar(P),raised,power(PO).
exp(P)-->pvar(P).

monomial(N)-->coef(N).
monomial(N)-->exp(N).
monomial(C*E)-->coef(C),times,exp(E).

minus-->[minus].
plus-->[plus].

polynomial(P)-->monomial(P).
polynomial(M+P)-->monomial(M),plus,polynomial(P).
polynomial(M-P)-->monomial(M),minus,polynomial(P).

text2poly(S,R):-split_string(S, ' ', '', L),aux_text2poly(L, R1), polynomial(R, R1, []).

aux_text2poly([], []).
aux_text2poly([H|T], [X|T1]):-atom_string(X,H),aux_text2poly(T, T1).