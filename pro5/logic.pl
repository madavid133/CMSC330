/*---------------------------------------------------------------

   CMSC 330 Project 6 - Maze Solver and SAT in Prolog

   NAME:

*/


%%%%%%%%%%%%%%%%%%%%%%
% Part 1 - Recursion %
%%%%%%%%%%%%%%%%%%%%%%

% ackermann - M and N are the two arguments, and R is the result. Cf http://mathworld.wolfram.com/AckermannFunction.html for the definition of the ackermann function

ackermann(0, N, R) :- R is N + 1.
ackermann(M, 0, R) :- P is M - 1, ackermann(P, 1, R).
ackermann(M, N, R) :- M > 0 , N > 0, P is N - 1, ackermann(M, P, R1), Q is M - 1, ackermann(Q, R1, R).

% prod - R is product of entries in list L

prod([], 1).
prod([H|T], R) :- 
	prod(T, R1), 
	R is R1 * H.

% fill - R is list of N copies of X

fill(0, _, []).
fill(N, X, [H|T]) :- 
	N > 0, 
	X = H,
	N1 is N - 1, 
	fill(N1, X, T).

% genN - R is value between 0 and N-1, in order

genN(N, R) :- genNhelper(N, R, 0).
genNhelper(N, R, X) :-
	X < N,
	(R = X;
	X1 is X + 1,
	genNhelper(N, R, X1)).

% genXY - R is pair of values [X,Y] between 0 and N-1, in lexicographic order

 
genXY(N,R) :- genXYhelper1(0, N, R).

genXYhelper1(X, N, R) :-
	X < N -> (genXYhelper2(X, 0, N, R); 
	X1 is X + 1, 
	genXYhelper1(X1, N, R)).

genXYhelper2(X, Y, N, R) :- 
	Y < N -> (R = [X, Y]; 
	Y1 is Y + 1, 
	genXYhelper2(X, Y1, N, R)).


% flat(L,R) - R is elements of L concatentated together, in order

flat(L,R) :- flathelper(L, [], R).
flathelper([], R, R).
flathelper([H|T], X, Y) :-
	(
    integer(H) -> 
    append(X, [H], Z),
	flathelper(T, Z, Y);
	append(X, H, Z),
	flathelper(T, Z, Y) 
    ).



% is_prime(P) - P is an integer; predicate is true if P is prime.

is_prime(P) :- 
	P > 1,
	is_primehelper(P, 2).
is_primehelper(P, X) :-
	X >= P -> true;
	0 is P mod X -> false;
    Y is X + 1,
	is_primehelper(P, Y). 


% in_lang(L) - L is a list of atoms a and b; predicate is true L is in the language accepted by the following CFG:
/*    
CFG 
S -> T | V
T -> UU
U -> aUb | ab
V -> aVb | aWb
W -> bWa | ba
*/

in_lang(L) :- in_Tang(L); in_Vlang(L).
in_Tang(L) :- append(L1, L2, L), in_Ulang(L1), in_Ulang(L2). 
in_Ulang([a|L]) :- append(L1, [b], L), in_Ulang(L1); append([], [b], L).
in_Vlang([a|L]) :- append(L1, [b], L), in_Vlang(L1); append(L2, [b], L), in_Wlang(L2).
in_Wlang([b|L]) :- append(L1, [a], L), in_Wlang(L1); append([], [a], L).


%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2 - Maze Solver %
%%%%%%%%%%%%%%%%%%%%%%%%

% stats(U,D,L,R) - number of cells w/ openings up, down, left, right
 
stats(U,D,L,R) :- maze(N,_,_,_,_), 
				findall(X, genXY(N,X),Lst),
				stathelp(Lst,U,L,D,R).
stathelp([],0,0,0,0).
stathelp([[X,Y]|T],U,L,D,R) :-
cell(X,Y,Dir,_),
stathelp(T,U1,L1,D1,R1),
(member(u,Dir) -> U is U1+1; U is U1),
(member(l,Dir) -> L is L1+1; L is L1),
(member(d,Dir) -> D is D1+1; D is D1),
(member(r,Dir) -> R is R1+1; R is R1).

% validPath(N,W) - W is weight of valid path N rounded to 4 decimal places

validPath(N,W) :- maze(_,_,_,EX,EY), 
				  path(N,SX,SY,Dir), 
				  validpathhelper(W1,SX,SY,EX,EY,Dir), 
				  round4(W1,W).

validpathhelper(0,_,_,_,_,[]).
validpathhelper(W,X,Y,EX,EY,[H|T]) :- cell(X,Y,Dirs,Wts), 
									  calPath(H,Dirs,Wts,W1), 
									  movement(X,Y,H,NX,NY), 
									  validpathhelper(W2,NX,NY,EX,EY,T), 
									  W is W1 + W2.							

calPath(D,[D|_],[V|_], W) :- W is V.
calPath(D,[_|T],[_|T3], W):- calPath(D,T,T3,W).

movement(X,Y,D,CX,CY) :-(D = u,CY is Y - 1,CX is X);
						(D = d,CY is Y + 1,CX is X);
						(D = l,CX is X - 1,CY is Y);
						(D = r,CX is X + 1,CY is Y).

round4(X,Y) :- T1 is X*10000, T2 is round(T1), Y is T2/10000.


% findDistance(L) - L is list of coordinates of cells at distance D from start

findDistance(L) :- maze(_,X,Y,_,_),
                   A = [[0,[X,Y]]],
                   constructor(A,[],[],L1),
                   sort(L1,L2),
                   startpoint(L2,[],R,B),
                   endpoint(R,B,L3),
                   sort(L3,L).
constructor([],P,L,L).
constructor([[D,[X,Y]]|T],P,L1, L) :- cell(X,Y,Dirs,_),
								   D1 is D + 1,
								   append([[X,Y]],P,P1),
								   !,process(D1,X,Y,Dirs,P1,T,Lst),
								   append([[D,[[X,Y]]]],L1,L2),
								   constructor(Lst,P1,L2,L).

process(D,X,Y,[],P,A,A).
process(D,X,Y,[H|T],P,A,Qf) :-  movement1(X,Y,H,CX,CY),
								!,(\+member([CX,CY],P) -> append(A,[[D,[CX,CY]]],Q1), process(D,X,Y,T,P,Q1,Qf);
								process(D,X,Y,T,P,A,Qf)).

startpoint([[D,[[X,Y]]]|T],[],R,L) :- append([[D,[[X,Y]]]],[],L), R = T.

endpoint([],L,L).
endpoint([[D,[[X,Y]]]|T], [[D,[[CX,CY]|T2]]|T3],L) :-  !,append([[X,Y]],[[CX,CY]],Cell),
													   append(Cell,T2,C1),
													   sort(C1,C2),
													   append([[D,C2]],T3,E),
													   endpoint(T,E,L). 
endpoint([[D,[[X,Y]]]|T],B,L) :- append([[D,[[X,Y]]]],B,R), endpoint(T,R,L).

member(Cell, [Cell|T]).
member(Cell, [H|T]) :- member(Cell,T).

movement1(X,Y,H,CX,CY) :- (H = u -> CY is Y - 1, CX is X);
						  (H = d -> CY is Y + 1, CX is X);
						  (H = l -> CX is X - 1, CY is Y);
						  (H = r -> CX is X + 1, CY is Y).


% solve - True if maze is solvable, fails otherwise.

solve :- maze(Size,SX,SY,EX,EY),
		 solvehelp([[SX,SY]],[],[EX,EY]).

solvehelp([H|_],_,H).
solvehelp([[X|[Y]]|T],A,L) :- append([[X,Y]],A,P1),
							  cell(X,Y,Dirs,_),
							  combine(X,Y,Dirs,T,P1,R),
							  solvehelp(R,P1,L).
				
combine(_,_,[],Q,_,Q).
combine(X,Y,[Dirs|T2],Q,A,R) :- movement2(X,Y,Dirs,CX,CY),
							  (\+member([CX,CY],A) -> append([[CX,CY]],Q,Q1);Q1 = Q),
							  combine(X,Y,T2,Q1,A,R).
							
movement2(X,Y,L,CX,CY) :- (L = u -> CY is Y - 1, CX is X);
						  (L = d -> CY is Y + 1, CX is X);
						  (L = l -> CX is X - 1, CY is Y);
						  (L = r -> CX is X + 1, CY is Y).


%%%%%%%%%%%%%%%%
% Part 3 - SAT %
%%%%%%%%%%%%%%%%

/*

% eval(F,A,R) - R is t if formula F evaluated with list of 
%                 true variables A is true, false otherwise

eval(F,A,R) :- fail.

% varsOf(F,R) - R is list of free variables in formula F

varsOf(F,R) :- fail.

% sat(F,R) - R is a list of true variables that satisfies F

sat(F,R) :- fail.

% Helper Function
% subset(L, R) - R is a subset of list L, with relative order preserved

subset([], []).
subset([H|T], [H|NewT]) :- subset(T, NewT).
subset([_|T], NewT) :- subset(T, NewT).
*/
