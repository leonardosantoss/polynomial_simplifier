%% Made by Leonardo Santos (201703489) and Lucas de Paula (201608440)

digits(0)-->[zero].
digits(0)-->['0'].
digits(1)-->[one].
digits(1)-->['1'].
digits(2)-->[two].
digits(2)-->['2'].
digits(3)-->[three].
digits(3)-->['3'].
digits(4)-->[four].
digits(4)-->['4'].
digits(5)-->[five].
digits(5)-->['5'].
digits(6)-->[six].
digits(6)-->['6'].
digits(7)-->[seven].
digits(7)-->['7'].
digits(8)-->[eight].
digits(8)-->['8'].
digits(9)-->[nine].
digits(9)-->['9'].
digits(10)-->[ten].
digits(10)-->['10'].
digits(11)-->[eleven].
digits(11)-->['11'].
digits(12)-->[twelve].
digits(12)-->['12'].


pvar(x)-->[x].
pvar(y)-->[y].
pvar(z)-->[z].

times-->[times].
raised-->[raised,to].
squared-->[squared].
cubed --> [cubed].

power(N)-->digits(N).

coef(N)-->digits(N).

exp(P^PO)-->pvar(P),raised,power(PO).
exp(P^2) -->pvar(P), squared.
exp(P^3)-->pvar(P), cubed.
exp(P)-->pvar(P).

monomial(N)-->coef(N).
monomial(N)-->exp(N).
monomial(C*E)-->coef(C),times,exp(E).

minus-->[minus].
minus-->['-'].
plus-->[plus].
plus-->['+'].

polynomial(P)-->monomial(P).
polynomial(M+P)-->monomial(M),plus,polynomial(P).
polynomial(M-P)-->monomial(M),minus,polynomial(P).

text2poly(S,R):-split_string(S, ' ', '', L),aux_text2poly(L, R1), polynomial(R, R1, []).

aux_text2poly([], []).
aux_text2poly([H|T], [X|T1]):-atom_string(X,H),aux_text2poly(T, T1).


polyplay:- writeln("Write your operation over a polynomial: ('help' to display instructions)"),writeln("Every query must end with a ENTER"),aux_polyplay(_,_).
aux_polyplay([],[]):-
    read_string(user_input, "\n", "\r", _, String), 
    split_string(String, " ", "", List), 
    aux_text2poly(List, R1), 
    verify(_,R1,[]).

%% verify user input and does the correct operation
verify(_) --> add_word, polynomial(R2), to_word, polynomial(R1),aux_add(R1, R2,_),aux_polyplay.
verify(_) --> add_word, polynomial(R2), to_word, polynomial(R1),as_word,name(Key),aux_add(R1,R2,R3),poly2remove(Key),poly2store(Key, R3),aux_polyplay.
verify(_) --> add_word, polynomial(R2), to_word, polynomial(R1),as_word,name(Key),aux_add(R1,R2,R3),poly2store(Key, R3),aux_polyplay.

verify(_) --> add_word, polynomial(R2), to_word, name(R1),storedPoly(R1,P1),aux_add(P1, R2,_),aux_polyplay.
verify(_) --> add_word, polynomial(R2), to_word, name(R1),as_word,name(Key),storedPoly(R1,P1),aux_add(P1, R2,R3),poly2remove(Key),poly2store(Key,R3),aux_polyplay.
verify(_) --> add_word, polynomial(R2), to_word, name(R1),as_word,name(Key),storedPoly(R1,P1),aux_add(P1, R2,R3),poly2store(Key,R3),aux_polyplay.

verify(_) --> add_word, name(R2), to_word, polynomial(R1),storedPoly(R2,P2),aux_add(R1, P2,_),aux_polyplay.
verify(_) --> add_word, name(R2), to_word, polynomial(R1),as_word,name(Key),storedPoly(R2,P2),aux_add(R1, P2,R3),poly2remove(Key),poly2store(Key,R3),aux_polyplay.
verify(_) --> add_word, name(R2), to_word, polynomial(R1),as_word,name(Key),storedPoly(R2,P2),aux_add(R1, P2,R3),poly2store(Key,R3),aux_polyplay.

verify(_) --> add_word, name(R2), to_word, name(R1),storedPoly(R2,P2),storedPoly(R1,P1),aux_add(P1, P2,_),aux_polyplay.
verify(_) --> add_word, name(R2), to_word, name(R1),as_word,name(Key),storedPoly(R2,P2),storedPoly(R1,P1),aux_add(P1, P2,R3),poly2remove(Key),poly2store(Key,R3),aux_polyplay.
verify(_) --> add_word, name(R2), to_word, name(R1),as_word,name(Key),storedPoly(R2,P2),storedPoly(R1,P1),aux_add(P1, P2,R3),poly2store(Key),aux_polyplay.

verify(_) --> bye_word,leave().
verify(_) --> help_word,help(),aux_polyplay.
verify(_) --> multiply_word, polynomial(R2), by_word, digits(R1),aux_mul(R2, R1),aux_polyplay.
verify(_) --> multiply_word, name(R2), by_word, digits(R1),storedPoly(R2,P2),aux_mul(P2, R1),aux_polyplay.
verify(_) --> multiply_word, digits(R2), by_word, polynomial(R1),aux_mul(R1, R2),aux_polyplay.
verify(_) --> multiply_word, digits(R2), by_word, name(R1),storedPoly(R1,P1),aux_mul(P1, R2),aux_polyplay.
verify(_) --> simplify_word, polynomial(R1),aux_simp(R1),aux_polyplay.
verify(_) --> simplify_word, name(R1),storedPoly(R1,P1), aux_simp(P1),aux_polyplay.
verify(_) --> show_word, polynomial(R1),aux_show(R1),aux_polyplay.
verify(_) --> show_word, name(R1),storedPoly(R1,P1) ,aux_show(P1),aux_polyplay.
verify(_) --> let_word,name(Key),be_word,polynomial(P1),poly2store(Key, P1),aux_polyplay.
verify(_) --> let_word,name(Key),be_word,polynomial(P1),poly2remove(Key),poly2store(Key,P1),aux_polyplay.
verify(_) --> forget_word,name(Key),poly2remove(Key),aux_polyplay.
verify(_) --> unknow_cmd,aux_polyplay.

% verify(_) --> add_word, polynomial(R2), to_word, polynomial(R1),dot ,aux_add(R1, R2),aux_polyplay.
% verify(_) --> add_word, polynomial(R2), to_word, name(R1),dot,storedPoly(R1,P1),aux_add(P1, R2),aux_polyplay.
% verify(_) --> add_word, name(R2), to_word, polynomial(R1),dot,storedPoly(R2,P2),aux_add(R1, P2),aux_polyplay.
% verify(_) --> add_word, name(R2), to_word, name(R1),dot,storedPoly(R2,P2),storedPoly(R1,P1),aux_add(P1, P2),aux_polyplay.
% verify(_) --> bye_word, dot,leave().
% verify(_) --> help_word, dot,help(),aux_polyplay.
% verify(_) --> multiply_word, polynomial(R2), by_word, digits(R1),dot ,aux_mul(R2, R1),aux_polyplay.
% verify(_) --> multiply_word, name(R2), by_word, digits(R1),dot,storedPoly(R2,P2),aux_mul(P2, R1),aux_polyplay.
% verify(_) --> multiply_word, digits(R2), by_word, polynomial(R1), dot,aux_mul(R1, R2),aux_polyplay.
% verify(_) --> multiply_word, digits(R2), by_word, name(R1),dot, storedPoly(R1,P1),aux_mul(P1, R2),aux_polyplay.
% verify(_) --> simplify_word, polynomial(R1), dot, aux_simp(R1),aux_polyplay.
% verify(_) --> simplify_word, name(R1),dot, storedPoly(R1,P1), aux_simp(P1),aux_polyplay.
% verify(_) --> show_word, polynomial(R1), dot ,aux_show(R1),aux_polyplay.
% verify(_) --> show_word, name(R1), dot,storedPoly(R1,P1) ,aux_show(P1),aux_polyplay.
% verify(_) --> let_word,name(Key),be_word,polynomial(P1),dot,poly2store(Key, P1),aux_polyplay.
% verify(_) --> let_word,name(Key),be_word,polynomial(P1),dot,poly2remove(Key),poly2store(Key,P1),aux_polyplay.
% verify(_) --> forget_word,name(Key), dot, poly2remove(Key),aux_polyplay.
% verify(_) --> unknow_cmd,aux_polyplay.

unknow_cmd(_,_):-writeln('Unknow command...'). 

:-dynamic
    storedPoly/4.

poly2store(Name,Poly,[],[]):-not(storedPoly(Name,_,_,_)),assert(storedPoly(Name,Poly,[],[])).
poly2remove(Name, [], []):-retract(storedPoly(Name,_,_,_)).
name(P) --> [P].

forget_word --> [forget].
as_word --> [as].
let_word --> [let].
be_word --> [be].
add_word --> [add].
to_word --> [to].
bye_word --> [bye].
help_word --> [help].
multiply_word --> [multiply]. 
simplify_word --> [simplify].
show_word --> [show].
by_word --> [by].
dot --> [.].
%% Sums two polynomials, prints the result and calls polyplay again, so the program can continue
% aux_add(R1,R2,_,_) :- write(">"),correct_parentesis(R1, Res1), correct_parentesis(R2, Res2),addpoly(Res1,Res2,R3), writeln(R3), writeln(" ") ,polyplay().
aux_add(R1,R2,R3,[],[]) :- write(">"),correct_parentesis(R1, Res1), correct_parentesis(R2, Res2),addpoly(Res1,Res2,R3), writeln(R3), writeln(" ").
%% Multiplies a polynomial by a Scalar
% aux_mul(R1,R2,_,_) :- write(">"), correct_parentesis(R1, Res1),scalepoly(Res1, R2, Res), writeln(Res),writeln(" "), polyplay().
aux_mul(R1,R2,[],[]) :- write(">"), correct_parentesis(R1, Res1),scalepoly(Res1, R2, Res), writeln(Res),writeln(" ").
%% simplifies given expression and calls polyplay again
% aux_simp(R1,_,_) :- write(">"),correct_parentesis(R1, Res1),simpoly(Res1, Res), writeln(Res), writeln(" ") ,polyplay().
aux_simp(R1,[],[]) :- write(">"),correct_parentesis(R1, Res1),simpoly(Res1, Res), writeln(Res), writeln(" ").
%% shows given polynomial
% aux_show(R1,_,_) :- write(">"),correct_parentesis(R1, Res1) ,writeln(Res1), writeln(" "), polyplay().
aux_show(R1,[],[]) :- write(">"),correct_parentesis(R1, Res1) ,writeln(Res1), writeln(" ").
%% leaves the program
leave([],[]) :- writeln("Bye!").

%% display a few instructions and calls polyplay again
help([], []) :- writeln("Available commands (with examples):"),  writeln("add: 'add two times x to three'"),
writeln("multiply: 'multiply two times x by three'"), 
writeln("simplify: 'simplify two times x squared plus two times x squared'"),
writeln("show: 'show two times x squared'"),
,writeln("bye: 'bye'"), writeln(" ") ,polyplay().


correct_parentesis(R, Term) :- format(chars(C), "~w", R), custom_format(C, Res), term_string(Term, Res).
custom_format(E,Ef):- delete(E,'(',E1),delete(E1,')',E2),delete(E2,' ',Ef).

%%%%%%%%%%%% PREVIOUS ASSIGNMENT %%%%%%%%%%%%%%%%



pvars([x,y,z]).
pvar(X):-pvars(V),member(X,V).

power(novarexp).
power(X):-pvar(X),!.
power(X^Y):-pvar(X),integer(Y),Y>=0,!.
power(-X^Y):-pvar(X),integer(Y),Y>=0,!.

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
monparts(-X, -1, X) :- pvar(X), !.
monparts(K*M, K, M) :- number(K), !. 
monparts(K, K, novarexp) :- number(K), !. %% case with only number
monparts(X, 1, X) :- pvar(X), !. %% case with var only

%%simplification of polynomes in expression format

simpoly(P, Res) :-polynomial(P), poly2list(P, L), simpoly_list(L, ResList), poly2list(Res, ResList),!.

%% simplification of polynomes in list format

simpoly_list(L1, L2):-simlist(L1,L),findall(X,custom_filter(X,L),L2).

%% get the next element and use getrem to sum the coefs of all elements with the same core, returns a list with a set of cores multiplied by the sum of coefs
simlist([H|L], [Sum|L2]):-number(H),!,monparts(H,Coef1,_),getrem(H, L, S, PS), Sum is Coef1 + S, simlist(PS,L2).
simlist([H|L], [A|L2]):-monparts(H,Coef1,Exp),!,getrem(H, L, S, PS), Sum is Coef1 + S,monparts(A,Sum,Exp), simlist(PS,L2).
simlist([H|L], [H|PS]):-simlist(L,PS),!.
simlist([], []):-!.

%% delete the first occurrence of a element in the list
deleteFirst([X|L],X,L):-!.
deleteFirst([H|T],X,[H|L]):-deleteFirst(T,X,L).

%% get all elements whose coef is different from 0
custom_filter(X,L):-member(X,L),not(monparts(X,0,_)).

%% recursively adds the coef of the next element with the same core as H and deletes it
getrem(H, L, Sum, Lf):-monparts(H,_,Exp),member(X,L),monparts(X,Coef2,Exp),deleteFirst(L,X,L2),getrem(H, L2, S, Lf),Sum is Coef2 + S,!.
getrem(_,L,0,L):-!.


%% poly2list(Polynomial, X) calls poly2list1
%% poly2list(X, [list of monomials]) calls poly2list2
poly2list(0, []):-!.
poly2list(P, L) :- var(P), poly2list2(P,L) , !.
poly2list(P,L) :- poly2list1(P,L).

%% Normal case poly2list1 where we receive a polynome in expression format and transforms it into a list of monomials
poly2list1(P - M, [Neg*Exp|T]):-monomial(M), polynomial(P),monparts(M, C, Exp), Neg is C*(-1), poly2list1(P, T),!.
poly2list1(P + M, [M|T]):-monomial(M), poly2list1(P, T), polynomial(P),!.
poly2list1(-M, [Neg*Exp]):-monomial(M),monparts(M, C, Exp), Neg is C*(-1),!.
poly2list1(M, [M]):-monomial(M),!.


%% Case poly2list where we transform a list of monomials into a polynome in expression format
% poly2list2(0,[]).
poly2list2(X, List):- poly2list1(Res, List), correct(Res, X).


%% Predicate that treats polynomes like 2*x^5+ -2*x^4  that are returned by calling poly2list(X, [2*x^5,-2*x^4])                   		   
correct(P, Res) :- 
compound2atom(P, P2),  %%  2x^5+ -2*x^4  -> '2*x^5+ -2*x^4' 
atom_string(P2, RX),   %% '2*x^5+ -2*x^4' -> "2*x^5+ -2*x^4" 
string_chars(RX, RXC), 	%% "2*x^5+ -2*x^4" -> [2,*,x,^,5,+, ,-,2,*,x,^,4]
deleteWrong(RXC, RXT), 	%% delete occurences of [+, ,-] (only supposed to have the -, not the + -)
flatten(RXT, RXS), 		%% deleteWrong gives a list with various sublists, need to be flattened into one list
string_chars(RXH, RXS), %% [2,*,x,^,5,-,2,*,x,^,4] -> "2*x^5-2*x^4"
term_string(Res, RXH).	%% "2*x^5-2*x^4" -> 2*x^5-2*x^4


deleteWrong([A1,A2|List], [Res]) :- A1 = '+', A2 = ' ', deleteWrong(List, Res), !.
deleteWrong([A1,A2|List], [A1|Res]) :- deleteWrong([A2|List],Res),!.
deleteWrong([A1],[A1]).

compound2atom(X, A) :- format(atom(A), '~w', X).

%% flattens a list that contains sublists
flatten([], []) :- !.
flatten([L|Ls], FlatL) :-
    !,
    flatten(L, NewL),
    flatten(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten(L, [L]).



%% adds two monnomials
addpoly(X,Y,R3):-poly2list(X,L1),poly2list(Y,L2),append(L1,L2,T),simpoly_list(T,R1),delete(R1,0,R2),poly2list(R3,R2).

%% Multiplies a polynome in expression format by a Scalar 
scalepoly(P, S, Res) :-poly2list(P, L), aux_scalepoly(L,S,Res1), poly2list(Res2, Res1), simpoly(Res2, Res).

aux_scalepoly([], _, []).
aux_scalepoly(List,1, List).
aux_scalepoly([K*X|List], S, [Y*X|Res]) :- number(K), power(X), Y is K*S, aux_scalepoly(List, S, Res). 
aux_scalepoly([X|List], S, [Y*X|Res]) :- power(X), Y is S, aux_scalepoly(List, S, Res).
aux_scalepoly([K|List], S, [Y|Res]) :- number(K), Y is K*S, aux_scalepoly(List, S, Res).














