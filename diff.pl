:-op(10,yfx,^).
:-op(9,fx,-).

% dx/dx->1
d(X,X,1):-!.

% dc/dx ->0
d(C,_,0):-atomic(C).

% d(-U)/dx ->-(dU/dx)
d(-U,X,-A):-d(U,X,A).

% d(U+V)/dx -> dU/dx+dV/dx
d(U+V,X,A+B):-d(U,X,A),d(V,X,B).

% d(U-V)/dx -> dU/dx-dV/dx
d(U-V,X,A-B):-d(U,X,A),d(V,X,B).

% d(cU)/dx -> c(dU/dx)
d(C*U,X,C*A):-atomic(C),C\=X,d(U,X,A),!.

% d(UV)/dx -> U(dV/dx)+V(dU/dx)
d(U*V,X,B*U+A*V):-d(U,X,A),d(V,X,B).

%d(U/V)/dx -> d(UV^-1)/dx
d(U/V,X,A):-d(U*V^(-1),X,A).

%d(U^c)/dx -> cU^(c-1)(dU/dx)
d(U^C,X,C*U^(C-1)*W):-atomic(C),C\=X,d(U,X,W).

%d(U^V)/dx -> U^V [V/U*(dU/dx) + ln(U)*(dV/dx)]
d(U^V,X,(U^V)*((V/U)*UA + ln(U)*VA)):-d(U,X,UA),d(V,X,VA).

%d(ln(U))/dx -> U^(-1)(dU/dx) 
d(ln(U),X,A/U):-d(U,X,A).

%d(sinU)/dx -> cos(U)(dU/dx) 
d(sin(U),X,A*cos(U)):-d(U,X,A).

%d(cos(U))/dx -> -sin(U)(dU/dx) 
d(cos(U),X,-A*sin(U)):-d(U,X,A).

%d(tan(U))/dx -> 1/cos(U)^2*(dU/dx) 
d(tan(U),X,A/cos(U)^2):-d(U,X,A).

%d(cot(U))/dx -> 1/sin(U)^2*(dU/dx) 
d(cot(U),X,-A/sin(U)^2):-d(U,X,A).

%d(exp(U))/dx -> exp(U)(dU/dx) 
d(exp(U),X,A*exp(U)):-d(U,X,A).

%d(sqrt(U))/dx -> 1/(2*sqrt(U))(dU/dx) 
d(sqrt(U),X,A/(2*sqrt(U))):-d(U,X,A).

simpl(E,E):-
  atomic(E),!.
  simpl(E,F):-
  E=..[Op,L,R],
  simpl(L,X),
  simpl(R,Y),
  s(Op,X,Y,F);
  E=..[Op,U],
  simpl(U,M),
  s(Op,M,F).

simpl(ln(E), ln(X)):-simpl(E, X).
simpl(sin(E), sin(X)):-simpl(E, X).
simpl(cos(E), cos(X)):-simpl(E, X).
simpl(tan(E), tan(X)):-simpl(E, X).
simpl(cot(E), cot(X)):-simpl(E, X).
simpl(sqrt(E), sqrt(X)):-simpl(E, X).
simpl(exp(E), exp(X)):-simpl(E, X).


s(+,X,0,X).
s(+,0,X,X).
s(+,X,Y,Z):-number(X),number(Y),Z is X+Y.
s(+,C1+X,C2,R):-number(C1),number(C2),C is C1+C2, s(+, C, X, R).
s(+,C1*X,C2*X,R):-number(C1),number(C2),C is C1+C2, s(*, C, X, R).
s(+,X*C1,C2*X,R):-number(C1),number(C2),C is C1+C2, s(*, C, X, R).
s(+,C1*X,X*C2,R):-number(C1),number(C2),C is C1+C2, s(*, C, X, R).
s(+,X*C1,X*C2,R):-number(C1),number(C2),C is C1+C2, s(*, C, X, R).
s(+,X,X,2*X).
s(+,X,Y,X+Y).

s(-,X,0,X).
s(-,0,X,-X).
s(-,X,Y,Z):-number(X),number(Y),Z is X-Y.
s(-,X+C1,C2,R):-number(C1),number(C2),C is C1-C2, s(+, X, C, R).
s(-,C1+X,C2,R):-number(C1),number(C2),C is C1-C2, s(+, X, C, R).
s(-,C1*X,C2*X,R):-number(C1),number(C2),C is C1-C2, s(*, C, X, R).
s(-,X*C1,C2*X,R):-number(C1),number(C2),C is C1-C2, s(*, C, X, R).
s(-,C1*X,X*C2,R):-number(C1),number(C2),C is C1-C2, s(*, C, X, R).
s(-,X*C1,X*C2,R):-number(C1),number(C2),C is C1-C2, s(*, C, X, R).
s(-,X,X,0).
s(-,X,Y,X-Y).

s(*,0,_,0).
s(*,_,0,0).
s(*,1,X,X).
s(*,X,1,X).
s(*,-1,X,-X).
s(*,X,-1,-X).
s(*,X*Y,W,X*Z):-number(Y),number(W),Z is Y*W,!.
s(*,X,Y,Z):-number(X),number(Y),Z is X*Y.
s(*,C1*X,C2,R):-number(C1),number(C2),C is C1*C2, s(*,C,X,R).
s(*,X,X,X^2).
s(*,X,Y,X*Y).

s(/,0,X,0):-X\=0.
s(/,X,1,X).
s(/,X,C1,R):-number(C1),C is 1/C1,s(*,C,X,R).
s(/,X,Y,Z):-number(X),number(Y),Y\=0,Z is X/Y.
s(/,X,X,1):-X\=0.
s(/,X,Y,X/Y) :- Y\=0.

s(^,0,_,0).
s(^,1,_,1).
s(^,_,0,1).
s(^,X,1,X).
s(^,X,C,1/R):-number(C),C<0,P is -C,s(^,X,P,R).
s(^,X,Y,X^Y).
s(^,X,Y,Z):-number(X),number(Y),Z is truncate(X**Y).

s(-,C,Z):-number(C), Z is -C.
s(-,-U,U).
s(-,U,-U).

dif(E,X,R):-d(E,X,R1),simpl(R1,R).