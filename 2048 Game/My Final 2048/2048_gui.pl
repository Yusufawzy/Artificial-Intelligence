% we don't want to calculate scores for the same board over and over'
% again
:- dynamic(getCurrentScore/2).

% Default depth is 4, but you can run it specifying any depth w han5lls bokra isa w mem 3alya bs better inny aksb.start :-
start:-write("Enter 1 to start with an initial state, 2 to enter your specific state:"),nl,
read(Init), ((Init is 1 -> play1);play2).

%==========================================================================================

%Start with this Board3 = [0,256,0,0,		0,0,0,256		,0,1024,0,0,		0,512,0,4],
% 3altol ana hena hadelo al board bta3ty oryba from the success
play1 :-
	Board3 = [256,0,0,0,		0,0,0,256		,0,1024,0,0,		0,512,0,4],
	Depth = 3,
	% generate(Board1,Board2),
	% generate(Board2,Board3),
	set_prolog_stack(global,limit(10*(10**9))),set_prolog_stack(local,limit(10*(10**9))),
	showBoard(Board3),
	loop(Board3, Depth).


% Get the input from the user line by line
play2 :-
	write('Enter the initial state of the game, each number followed by a dot'),nl,
	write('11:'),read(X1),write('12:'),read(X2),write('13:'),read(X3),write('14:'),read(X4),nl,
	write('21'),read(X5),write('22:'),read(X6),write('23:'),read(X7),write('24:'),read(X8),nl,
	write('31:'),read(X9),write('32:'),read(X10),write('33:'),read(X11),write('34:'),read(X12),nl,
	write('41:'),read(X13),write('42:'),read(X14),write('43:'),read(X15),write('44:'),read(X16),nl,
	
	Board1 = [X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12,X13, X14, X15, X16],
	Depth = 3,
	% mabd2yen kda zaawwd al stack bta3k ll2a5er
	set_prolog_stack(global,limit(10*(10**9))),set_prolog_stack(local,limit(10*(10**9))),
	showBoard(Board1),
	loop(Board1, Depth).

%==========================================================================================
%takes a board and returns the new number I'll put into it'
zeros([],_,[]).
zeros([H|T],W,L):-
	%if the current head is zero then add this idx to the array
	H is 0 -> (W1 is W + 1,zeros(T ,W1,L1),append(L1,[W],L))
			; ( W1 is W +1,zeros(T ,W1,L)).

randd(List,N):-
	random_member(N,List).
% it takes in the second paramter the index of the number it will replace, starting from 1 and from the end
put2([],_,_,[]).	
put2([H1|T1],P,W,[H1|T2]):-
	W \= P,
	W1 is W + 1, % hanb3t al2rqam indexed mn odam le wra zy al3ady
	put2(T1,P,W1,T2).
put2([H1|T1],P,W,[H2|T2]):-
	H2 is 2, %replace this P idx with 2 
	W == P,
	W1 is W +1 ,
	put2(T1,P,W1,T2).

% generate returns an index contains zero in the given array and put bdalo 2
generate(B1,B2):-
	zeros(B1,0,NoOfZeros), %zeros returns a list contains indexes of the zeros
	randd(NoOfZeros,N), %returns a reandom number from the given idinces  
	put2(B1,N,0,B2).
%==========================================================================================
loop(B,Depth):-
	max_list(B, 2048),nl,writeln('FINISHEEEEEEeD'),!,true.
loop(B,Depth):-
	moveLeft(B, B1),moveRight(B, B2),moveUp(B, B3),moveDown(B, B4),
	equal(B, B1),equal(B, B2),equal(B,B3),equal(B,B4),nl,writeln('GAME OVER'),!,true.
loop(B, Depth) :-
	%get the states of the 4 moves in B1,B2,B3,B4
	moveLeft(B, B1),moveRight(B, B2),moveUp(B, B3),moveDown(B, B4),
	%if you are winner dinner then %FINISHEEEEEEeD and abort
	
	%if all are equal to before so return that 5alas kda we're done'	
	evaluate(B, B1, Depth, N1),
	evaluate(B, B2, Depth, N2),
	evaluate(B, B3, Depth, N3),
	evaluate(B, B4, Depth, N4),

	best(N1,N2,N3,N4, Move),
	move(B, Move, NewBoard), %I select "Move" so perform it for me
	showBoard(NewBoard),
	% Here I already got the new Best Board after ai move and am calling
	% my self again till I hit one of the first two conditions of game over or win
	loop(NewBoard, Depth),!.
	
equal([],[]).
equal([H1|T1],[H2|T2]) :-
	H1 == H2,
	equal(T1,T2).
%0---R 1---D 2---U 3---L
%Each time do the move then make a new number
move(Board,2, NewBoard) :-
	writeln('Up--'),
	moveUp(Board, B)->generate(B, NewBoard).
move(Board, 3, NewBoard) :-
	writeln('Left--'),
	moveLeft(Board, B)->generate(B, NewBoard).
move(Board, 1, NewBoard) :-
	writeln('Down--'),
	moveDown(Board, B)->generate(B, NewBoard).
move(Board, 0, NewBoard) :-
	writeln('Right--'),
	moveRight(Board, B)->generate(B, NewBoard).

%THE_MINIMAX_PAAAAAAAAAAAAAART
% the move with the highest score is selected
best(ScoreL, ScoreR, ScoreU, ScoreD,0) :-
	ScoreR >= ScoreU,ScoreR >= ScoreD,ScoreR >= ScoreL.
best(ScoreL, ScoreR, ScoreU, ScoreD,2) :-
	ScoreU >= ScoreR,ScoreU >= ScoreD,ScoreU >= ScoreL.
best(ScoreL, ScoreR, ScoreU, ScoreD, 1) :-
	ScoreD >= ScoreU,ScoreD >= ScoreR,ScoreD >= ScoreL.
best(_, _, _, _, 3).

% if the move is not possible, it gets a score of 0
evaluate(Board, NewBoard, _, 0) :-
	equal(Board, NewBoard).
% this, along with the try and evalMoves predicates search the game
% space for the best move
evaluate(_, Board, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	getCurrentScore(Board, CurrScore),
	asserta(getCurrentScore(Board, CurrScore)),
	try(Board, 0, NewLevel, S0),try(Board, 1, NewLevel, S1),
	try(Board, 2, NewLevel, S2),try(Board, 3, NewLevel, S3),
	try(Board, 4, NewLevel, S4),try(Board, 5, NewLevel, S5),
	try(Board, 6, NewLevel, S6),try(Board, 7, NewLevel, S7),
	try(Board, 8, NewLevel, S8),try(Board, 9, NewLevel, S9),
	try(Board, 10, NewLevel, S10),try(Board, 11, NewLevel, S11),
	try(Board, 12, NewLevel, S12),try(Board, 13, NewLevel, S13),
	try(Board, 14, NewLevel, S14),try(Board, 15, NewLevel, S15),
	Score is 10*CurrScore+S0+S1+S2+S3+S4+S5+S6+S7+S8+S9+S10+S11+S12+S13+S14+S15.

%if there is zero in this place, so replace with (2) and see what is the new results
try([0|T], 0, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	betterofMoves([2|T],NewLevel,Score).
try([A1,0|T], 1, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	betterofMoves([A1,2|T],NewLevel,Score).
try([A1,A2,0|T], 2, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	betterofMoves([A1,A2,2|T],NewLevel,Score).
try([A1,A2,A3,0|T], 3, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	betterofMoves([A1,A2,A3,2|T],NewLevel,Score).
try([A1,A2,A3,A4,0|T], 4, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	betterofMoves([A1,A2,A3,A4,2|T],NewLevel,Score).
try([A1,A2,A3,A4,B1,0|T], 5, Level, Score) :-
	Level >= 0,NewLevel is Level - 1,
	betterofMoves([A1,A2,A3,A4,B1,2|T],NewLevel,Score).
try([A1,A2,A3,A4,B1,B2,0|T], 6, Level, Score) :-
	Level >= 0,NewLevel is Level - 1,
	betterofMoves([A1,A2,A3,A4,B1,B2,2|T],NewLevel,Score).
try([A1,A2,A3,A4,B1,B2,B3,0|T], 7, Level, Score) :-
	Level >= 0,NewLevel is Level - 1,
	betterofMoves([A1,A2,A3,A4,B1,B2,B3,2|T],NewLevel,Score).

try([A1,A2,A3,A4,B1,B2,B3,B4,0|T], 8, Level, Score) :-
	Level >= 0,NewLevel is Level - 1,
	betterofMoves([A1,A2,A3,A4,B1,B2,B3,B4,2|T],NewLevel,Score).
try([A1,A2,A3,A4,B1,B2,B3,B4,C1,0|T], 9, Level, Score) :-
	Level >= 0,NewLevel is Level - 1,
	betterofMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,2|T],NewLevel,Score).
try([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,0|T], 10, Level, Score) :-
	Level >= 0,NewLevel is Level - 1,
	betterofMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,2|T],NewLevel,Score).
try([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,0|T], 11, Level, Score) :-
	Level >= 0,NewLevel is Level - 1,
	betterofMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,2|T],NewLevel,Score).
try([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,0|T], 12, Level, Score) :-
	Level >= 0,NewLevel is Level - 1,
	betterofMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,2|T],NewLevel,Score).
try([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,0|T], 13, Level, Score) :-
	Level >= 0,NewLevel is Level - 1,
	betterofMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,2|T],,NewLevel,Score).
try([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,0|T], 14, Level, Score) :-
	Level >= 0,NewLevel is Level - 1,
	betterofMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,2|T],NewLevel,Score).
try([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,0], 15, Level, Score) :-
	Level >= 0,NewLevel is Level - 1,
	betterofMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,2],NewLevel,Score).
try(_, _, _, 0).

betterofMoves(B, Level, Score) :-
	moveLeft(B, BL),evaluate(B, BL, Level, ScoreL),
	moveRight(B, BR),evaluate(B, BR, Level, ScoreR),
	moveUp(B, BU),evaluate(B, BU, Level, ScoreU),
	moveDown(B, BD),evaluate(B, BD, Level, ScoreD),
	Score is (ScoreD+ScoreU+ScoreR+ScoreL).

% calculates the score for a board. The score is just the sum of squares
% of all tile values
getCurrentScore(Board, Score) :-
	sum_list(Board, Score).

% rotate the board to the right
rotateRight([X11,X12,X13,X14,	X21,X22,X23,X24,	X31,X32,X33,X34,	X41,X42,X43,X44],
		    [X41,X31,X21,X11,   X42,X32,X22,X12,	X43,X33,X23,X13,	X44,X34,X24,X14]).

% rotate the board to the left
rotateLeft([X11,X12,X13,X14,	X21,X22,X23,X24,	X31,X32,X33,X34,	X41,X42,X43,X44],
		    [X14,X24,X34,X44,   X13,X23,X33,X43,	X12,X22,X32,X42,	X11,X21,X31,X41]).

% only moveLeft is actually implemented. Every other move just rotates
% the board first and then uses moveLeft.
moveUp(Board, NewBoard):-
	rotateLeft(Board, Temp1),
	moveLeft(Temp1, Temp2),
	rotateRight(Temp2, NewBoard).

moveDown(Board, NewBoard):-
	rotateRight(Board, Temp1),
	moveLeft(Temp1, Temp2),
	rotateLeft(Temp2, NewBoard).

moveRight(Board, NewBoard):-
	rotateLeft(Board, Temp1),
	rotateLeft(Temp1, Temp2),
	moveLeft(Temp2, Temp3),
	rotateRight(Temp3, Temp4),
	rotateRight(Temp4, NewBoard).
%==========================================================================================
%moveLeft([2,4,4,8],R).
moveLeft([],[]).
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]):-
	moveLeft1([X1,X2,X3,X4], [N1,N2,N3,N4],0),moveLeft(X,N).
moveLeft1([H1,H2|T],Final,Iter):- %The last return will be Final
	%if count zero means examine the first two elements
	%if they are equal or one of them is zero then add and don't change the count you may need to iterate again thru these two numbers'
	Iter<3,
	((H1 == 0 , H2==0)->
		 (append(T,[0,0],B1),
		   Iter1 is Iter + 1,moveLeft1(B1,Final,Iter1) );
	(H1==0)->
		 (append([H2],T,B),append(B,[0],B1), 
		   Iter1 is Iter + 1,moveLeft1(B1,Final,Iter1) );
	(H2==0)->
		 (append([H1],T,B),append(B,[0],B1),
		 Iter1 is Iter + 1 ,moveLeft1(B1,Final,Iter1));
	( H1 == H2 ) ->
		 (Res is H1 + H2 ,
		 append([Res],T,B),append(B,[0],B1),moveLeft2(B1,Final,0));
	(append([H1,H2],T,B1), moveLeft2(B1,Final,0)     )),!. 
	%B1 is the modified array in my call, but the Final will be the Final of all
moveLeft1(B1,Final,Iter):-
	moveLeft2(B1,Final,0).

moveLeft2([H1,H2,H3,H4],Final,Iter):-
	Iter < 2,
	((H2 == 0 , H3==0)->
		 (append([H1],[H4,0,0],B1),
		 Iter1 is Iter + 1 ,moveLeft2(B1,Final,Iter1));
	(H2==0)->
		 (append([H1],[H3,H4,0],B1),
		  Iter1 is Iter + 1 ,moveLeft2(B1,Final,Iter1));
	(H3==0)->
		 (append([H1],[H2,H4,0],B1),
		 Iter1 is Iter + 1 ,moveLeft2(B1,Final,Iter1));
	( H2 == H3 ) ->
		 (Res is H2 + H3 ,
		 append([H1],[Res,H4,0],B1),moveLeft3(B1,Final,0));
	(append([H1,H2],[H3,H4],B1) ,moveLeft3(B1,Final,0)   )  ),!.
moveLeft2(B1,Final,Iter):-
	moveLeft3(B1,Final,0).

moveLeft3([H1,H2,H3,H4],B1,Iter):-
	Iter < 1,
	((H4 == 0 , H3==0)->
		 (append([H1,H2],[0,0],B1) );
	(H3==0)->
		 (append([H1,H2],[H4,0],B1));
	(H4==0)->
		 (append([H1,H2],[H3,0],B1));
	( H3 == H4 ) ->
		 (Res is H3 + H4 ,
		 append([H1,H2],[Res,0],B1));
	(append([H1,H2],[H3,H4],B1)			)),!. 
%==========================================================================================

% show the current board on screen
showBoard(List):-write(List),write('--').

% prints the number in formatted output, keeping everything aligned
printNumber(N) :-
	N >= 1000,
	write(' '),write(N).
printNumber(N) :-
	N >= 100,
	write('  '),write(N).
printNumber(N) :-
	N >= 10,
	write('   '),write(N).
printNumber(N) :-
	N == 0,
	write('    _').
printNumber(N) :-
	write('    '),write(N).
