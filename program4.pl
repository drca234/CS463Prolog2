%citypath takes the start, end, maze, and a path variable.
%if the start x is the same as the end x and the start y
%is the same as the end y, the solution is found unless
%there is a jenny blocking the square. If this is the case, 
%the puzzle is unsolvable and the program returns no. 

%Also good to note: The output will be in row, column format.

%Included in order to use nth1
:- use_module( library( listut ) ).

:- dynamic walkable/2.

mazepath( X, Y, Maze, Path, Score ) :-
	%Explore will always return false, as false means it explored every tile it could.
	%This is why we have the negation of explore. Without it, solvable will never be called.
	\+explore( X, Y, Maze), 
	\+solvable( Maze ), !, fail.
	
mazepath( X, Y, Maze, Path, Score ) :-
	findTile( EggX, EggY, Maze, 'e' ),
	citypath( X, Y, EggX, EggY, Maze, StartToEgg ),
	findTile( PikaX, PikaY, Maze, 'p' ),
	citypath( EggX, EggY, PikaX, PikaY, Maze, EggToPika ),
	findTile( MasterX, MasterY, Maze, 'mb' ),
	citypath( PikaX, PikaY, MasterX, MasterY, Maze, PikaToMaster ),
	findTile( MewtwoX, MewtwoY, Maze, 'mt' ), 
	citypath( MasterX, MasterY, MewtwoX, MewtwoY, Maze, MasterToEnd ),
	
	removehead( EggToPika, ToPika ), %Egg is already included in StartToEgg, so remove it from this list.
	removehead( PikaToMaster, ToMaster ), %Remove double inclusion of Pika space.
	removehead( MasterToEnd, ToEnd ), %Remove double inclusion of Master space.
	
	%The following appends put together the individual paths to one full path.
	append( StartToEgg, ToPika, StartToPika ),
	append( StartToPika, ToMaster, StartToMaster), 
	append( StartToMaster, ToEnd, Path ),
	
	%Score is 11 because you only need one Pikachu and one hatched egg, which
	%must have hatched between the egg and the Mewtwo.
	Score is 11,
	
	%We retract every space in walkable so subsequent runs will work.
	retract( walkable( Every, Space ) ).
	
removehead( [_|Tail], Tail). %Idea from http://stackoverflow.com/questions/5259941/how-to-remove-the-first-element-in-list
	

explore( X, Y, Maze ) :- 
	NewX is X + 1, 
	validMove( NewX, Y, Maze ),
	\+walkable( NewX, Y ), 
	assert( walkable( NewX, Y ) ),
	explore( NewX, Y, Maze ).
	
explore( X, Y, Maze ) :- 
	NewY is Y + 1, 
	validMove( X, NewY, Maze ),
	\+walkable( X, NewY ), 
	assert( walkable( X, NewY ) ),
	explore( X, NewY, Maze ).

explore( X, Y, Maze ) :- 
	NewX is X - 1, 
	validMove( NewX, Y, Maze ),
	\+walkable( NewX, Y ), 
	assert( walkable( NewX, Y ) ),
	explore( NewX, Y, Maze ).
	
explore( X, Y, Maze ) :- 
	NewY is Y - 1, 
	validMove( X, NewY, Maze ),
	\+walkable( X, NewY ), 
	assert( walkable( X, NewY ) ),
	explore( X, NewY, Maze ).
	
solvable( Maze ) :- 
	findTile( _, _, Maze, 'p' ),
	findTile( _, _, Maze, 'e' ),
	findTile( _, _, Maze, 'mb' ),
	findTile( _, _, Maze, 'mt' ). 
	
findTile( X, Y, Maze, Tile ) :-
	walkable( X, Y ), 
	nth1( Y, Maze, Column ), 
	nth1( X, Column, Tile ).

%This calls a version of the function that has a variable to store the current path.
citypath( X1, Y1, X2, Y2, Maze, Path ) :-
	CurrentPath = [ [ X1, Y1 ] ],
	citypath( X1, Y1, X2, Y2, Maze, Path, CurrentPath ).

%Checks to see we are at the destination, then if there is a Jenny at the destination, then returns the path if no Jenny.
citypath( X1, Y1, X2, Y2, Maze, Path, CurrentPath ) :-
	X1 == X2, Y1 == Y2, 
	!, validMove( X1, Y1, Maze ),
	reverse( CurrentPath, Path ).

%Tries to move in the X + 1 direction. Fails if that location was already explored, or if the location is invalid. 
citypath( X1, Y1, X2, Y2, Maze, Path, CurrentPath ) :- 
	NewX is X1 + 1, validMove( NewX, Y1, Maze ),
	\+member( [ NewX, Y1 ], CurrentPath ), 
	append( [ [ NewX, Y1 ] ], CurrentPath, NewPath ), 
	citypath( NewX, Y1, X2, Y2, Maze, Path, NewPath ).

%Tries to move in the Y + 1 direction. Fails if that location was already explored, or if the location is invalid. 
citypath( X1, Y1, X2, Y2, Maze, Path, CurrentPath ) :- 
	NewY is Y1 + 1, validMove( X1, NewY, Maze ), 
	\+member( [ X1, NewY ], CurrentPath ), 
	append( [ [ X1, NewY ] ], CurrentPath, NewPath ), 
	citypath( X1, NewY, X2, Y2, Maze, Path, NewPath ).

%Tries to move in the X - 1 direction. Fails if that location was already explored, or if the location is invalid. 
citypath( X1, Y1, X2, Y2, Maze, Path, CurrentPath ) :- 
	NewX is X1 - 1, validMove( NewX, Y1, Maze ), 
	\+member( [ NewX, Y1 ], CurrentPath ), 
	append( [ [ NewX, Y1 ] ], CurrentPath, NewPath ), 
	citypath( NewX, Y1, X2, Y2, Maze, Path, NewPath ).

%Tries to move in the Y - 1 direction. Fails if that location was already explored, or if the location is invalid. 
citypath( X1, Y1, X2, Y2, Maze, Path, CurrentPath ) :- 
	NewY is Y1 - 1, validMove( X1, NewY, Maze ), 
	\+member( [ X1, NewY ], CurrentPath ), 
	append( [ [ X1, NewY ] ], CurrentPath, NewPath ), 
	citypath( X1, NewY, X2, Y2, Maze, Path, NewPath ).

%If the coordinate in question is out of range, this fails through one of the nth1 calls. 
%If the coordinate is valid, it checks for Jenny, and fails if she is there.
%If the coordinate is valid and there is no Jenny, the space can be moved to.
validMove( X1, Y1, Maze ) :- 
	nth1( Y1, Maze, Column ), nth1( X1, Column, IsJenny ), IsJenny \== 'j'.