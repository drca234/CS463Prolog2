% Daniel Carroll
% 11/4/2016
% Prolog Assignment 2016

% The coordinates in this assignment are inverted compared to my last assignment.
% I did my best to implement it the way Dr. Goldsmith said to on the discussion
% board, with the first list of lists sharing the same Y value, which does make
% more sense. 

% The assumptions made by this program not specified by the
% discussion board or project description are as follow:
% 1. Though there may be multiple eggs or Pikachus, only one 
%   of each is aqcuired whilest walking through the maze. This
%   program is not greedy, and seeks not to take more eggs or 
%   Pikachus than are needed for completion. 

% This assignment, delightfully, is entirely backwards compatible with the
% previous assignment. The existing code was added to in a way to take advantage
% of the code that was previously written, and backwards compatibility was
% an unintended side effect, minus the change to the validMove predicate.

% Included in order to use nth1
:- use_module( library( listut ) ).

% Dynamic is required to have the predicates assert facts.
:- dynamic walkable/2.

% This is the predicate to supply with your starting X and Y coordinates,
% as well as your Maze. It will output your Path and Score, if called with
% uninstantiated variables for the two respective parameters. 
mazepath( X, Y, Maze, Path, Score ) :-
	% Explore will always return false, as false means it explored every tile it could.
	% This is why we have the negation of explore. Without it, solvable will never be called.
	\+explore( X, Y, Maze), 
	\+solvable( Maze ), !, fail.
	
% If the maze is solvable, this predicate will solve it.
mazepath( X, Y, Maze, Path, Score ) :-
	
	% We first find an egg's X and Y coordinates.
	findTile( EggX, EggY, Maze, 'e' ),
	
	% We then find a path from the start to the egg. 
	citypath( X, Y, EggX, EggY, Maze, StartToEgg ),
	
	% The next three sets of findTile and citypath find their specified coordinates.
	findTile( PikaX, PikaY, Maze, 'p' ),
	citypath( EggX, EggY, PikaX, PikaY, Maze, EggToPika ),
	findTile( MasterX, MasterY, Maze, 'mb' ),
	citypath( PikaX, PikaY, MasterX, MasterY, Maze, PikaToMaster ),
	findTile( MewtwoX, MewtwoY, Maze, 'mt' ), 
	citypath( MasterX, MasterY, MewtwoX, MewtwoY, Maze, MasterToEnd ),
	
	% A reminder that the Path variable returned by citypath includes the 
	% starting tile. Because of this, we must remove the first element of each
	% list after the first list. 
	removehead( EggToPika, ToPika ), % Egg's coordinates are already included in StartToEgg, so remove them from this list.
	removehead( PikaToMaster, ToMaster ), % Prevent double inclusion of Pika space.
	removehead( MasterToEnd, ToEnd ), % Prevent double inclusion of Master space.
	
	% The following appends combine the individual paths into one full path.
	append( StartToEgg, ToPika, StartToPika ),
	append( StartToPika, ToMaster, StartToMaster), 
	append( StartToMaster, ToEnd, Path ),
	
	% Score is 11 because you only need one Pikachu and one hatched egg, which
	% must have hatched between the egg and the Mewtwo. 
	Score is 11,
	
	% We retract every space in walkable so subsequent runs will work.
	retract( walkable( Every, Space ) ).
	
removehead( [_|Tail], Tail). % Idea from http://stackoverflow.com/questions/5259941/how-to-remove-the-first-element-in-list
% There may have been a built in function to do the same, but this works. 

% Explore takes a starting coordinate and ventures through the maze until it has
% successfully explored every space reachable from the starting space.
% This procedure may seem slightly redundant, as it does not check the tile you 
% initially start on, but this will only be a problem if the starting tile is
% walkable, but surrounding tiles are not. That case would fail anyway though,
% so I don't feel this is a bug worth fixing.
explore( X, Y, Maze ) :- 
	NewX is X + 1, 
	validMove( NewX, Y, Maze ),
	
	% If the valid coordinate in question has not yet been asserted
	% to be a walkable tile, we assert it and then call explore
	% on the new coordinate.
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
	
% Checks for a Pikachu, an egg, a Masterball, and a Mewtwo in the 
% walkable spaces.
solvable( Maze ) :- 
	% We do not actually care what tile these objectives are on, 
	% so we let Prolog choose them, verify they are correct, then 
	% dispose of the satisfying coordinates. 
	findTile( _, _, Maze, 'p' ),
	findTile( _, _, Maze, 'e' ),
	findTile( _, _, Maze, 'mb' ),
	findTile( _, _, Maze, 'mt' ). 
	
% This predicate is the main reason I did not pull my hair out doing this assignment.
% If you specify a particular Tile type in your Maze, it will check the walkable
% tiles to see if the particular Tile exists. 
findTile( X, Y, Maze, Tile ) :-
	walkable( X, Y ),
	nth1( Y, Maze, Column ), 
	nth1( X, Column, Tile ).

% This calls a version of the function that has a variable to store the current path.
citypath( X1, Y1, X2, Y2, Maze, Path ) :-
	CurrentPath = [ [ X1, Y1 ] ],
	citypath( X1, Y1, X2, Y2, Maze, Path, CurrentPath ).

% Checks to see we are at the destination, then if there is a Jenny at the destination, then returns the path if no Jenny.
citypath( X1, Y1, X2, Y2, Maze, Path, CurrentPath ) :-
	X1 == X2, Y1 == Y2, 
	!, validMove( X1, Y1, Maze ),
	reverse( CurrentPath, Path ).

% Tries to move in the X + 1 direction. Fails if that location was already explored, or if the location is invalid. 
citypath( X1, Y1, X2, Y2, Maze, Path, CurrentPath ) :- 
	NewX is X1 + 1, validMove( NewX, Y1, Maze ),
	\+member( [ NewX, Y1 ], CurrentPath ), 
	append( [ [ NewX, Y1 ] ], CurrentPath, NewPath ), 
	citypath( NewX, Y1, X2, Y2, Maze, Path, NewPath ).

% Tries to move in the Y + 1 direction. Fails if that location was already explored, or if the location is invalid. 
citypath( X1, Y1, X2, Y2, Maze, Path, CurrentPath ) :- 
	NewY is Y1 + 1, validMove( X1, NewY, Maze ), 
	\+member( [ X1, NewY ], CurrentPath ), 
	append( [ [ X1, NewY ] ], CurrentPath, NewPath ), 
	citypath( X1, NewY, X2, Y2, Maze, Path, NewPath ).

% Tries to move in the X - 1 direction. Fails if that location was already explored, or if the location is invalid. 
citypath( X1, Y1, X2, Y2, Maze, Path, CurrentPath ) :- 
	NewX is X1 - 1, validMove( NewX, Y1, Maze ), 
	\+member( [ NewX, Y1 ], CurrentPath ), 
	append( [ [ NewX, Y1 ] ], CurrentPath, NewPath ), 
	citypath( NewX, Y1, X2, Y2, Maze, Path, NewPath ).

% Tries to move in the Y - 1 direction. Fails if that location was already explored, or if the location is invalid. 
citypath( X1, Y1, X2, Y2, Maze, Path, CurrentPath ) :- 
	NewY is Y1 - 1, validMove( X1, NewY, Maze ), 
	\+member( [ X1, NewY ], CurrentPath ), 
	append( [ [ X1, NewY ] ], CurrentPath, NewPath ), 
	citypath( X1, NewY, X2, Y2, Maze, Path, NewPath ).

% If the coordinate in question is out of range, this fails through one of the nth1 calls. 
% If the coordinate is valid, it checks for Jenny, and fails if she is there.
% If the coordinate is valid and there is no Jenny, the space can be moved to.
validMove( X1, Y1, Maze ) :- 
	nth1( Y1, Maze, Column ), 
	nth1( X1, Column, IsJenny ), 
	IsJenny \== 'j', % check if there is a Jenny for a mazepath call.
	IsJenny \== 1. % check if there is a Jenny for a citypath call.
	