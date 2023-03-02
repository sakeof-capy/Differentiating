

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

%d(lnU)/dx -> U^(-1)(dU/dx) 
d(log(U),X,A*U^(-1)):-d(U,X,A).

d(U(V))/dx -> (dV/dU)*dU/dx
d(U_V_X,X,DV*DU):-
  U_V_X=..[U,V_X],
  d(U_V_X,V_X,DU),
  d(V_X,X,DV).

simpl(E,E):-atomic(E),!.
simpl(E,F):-
E=..[Op,L,R],
simpl(L,X),
simpl(R,Y),
s(Op,X,Y,F).
s(+,X,0,X).
s(+,0,X,X).
s(+,X,Y,Z):-integer(X),integer(Y),Z is X+Y.
s(+,X+Y,W,X+Z):-integer(Y),integer(W),Z is Y+W.
s(+,X,Y,X+Y).

s(*,0,_,0).
s(*,_,0,0).
s(*,1,X,X).
s(*,X,1,X).
s(*,X*Y,W,X*Z):-integer(Y),integer(W),Z is Y*W.
s(*,X,Y,Z):-integer(X),integer(Y),Z is X*Y.
s(*,X,Y,X*Y).

s(-,X,0,X).
s(-,X,Y,X-Y).
s(-,X,Y,Z):-integer(X),integer(Y),Z is X-Y.


s(^,0,_,0).
s(^,1,_,1).
s(^,_,0,1).
s(^,X,Y,X^Y).
s(^,X,Y,Z):-integer(X),integer(Y),Z is truncate(X**Y).

dif(E,X,R):-d(E,X,R1),simpl(R1,R).