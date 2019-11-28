%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This is your first homework problem!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Everyone knows the puzzle of a farmer with a fox, a goose, and a bag of corn
% to take to the market. She has to cross the river with a boat that can carry
% one object, if she ever leaves the fox with the goose, the goose is eaten, or
% the goose with the corn the corn is eaten, and she has to get everything across
% the river. This is a generalization of that problem.

% The farmer has f foxes, g geese, and c bags of corn on the west side of the
% river. She has a boat that can carry k objects (any mix of types is allowable)
% and the time to make t trips. Note that the first trip is west to east, then
% the second trip is east to west, then the third trip is west to east, etc. When
% ever the farmer leaves some goods alone on either side of the river then by the
% time she returns the following happens:

% - if there is only one kind of good, nothing.

% - if there are only foxes and corn, out of boredom one fox eats a bag of corn,
%   its stomach explodes and it dies.

% - if there are foxes and geese,

%   * if there are more foxes than geese one fox dies in argument over geese, no
%     geese die, and no geese eat any corn

%   * if there are no more foxes than geese, each fox eats a goose, and no geese
%     eat any corn.

% - if there are no foxes but there is geese and corn,

%   * if there is no more geese than corn each goose eats a bag of corn
  
%   * otherwise all the geese fight, one dies and one bag of corn is eaten.

% Once she has completed her last trip then the farmer can take the goods from
% the east side to the market where she receives pf for each fox, pg for each
% goose and pc for each corn. The aim is to maximize profit.


int: f; % number of foxes
int: g; % number of geese
int: c; % number of bags of corn
int: k; % capacity of boat (number of fox, geese and bags of corn)
set of int: Cap = 0..k;
int: t; % maximum number of trips
set of int: Trips = 1..t;
int: pf; % profit per fox
int: pg; % profit per goose
int: pc; % profit per bag of corn

% Decision variables

array[Trips] of var Cap: fox;   % number of foxes taken
array[Trips] of var Cap: geese; % number of geese taken
array[Trips] of var Cap: corn;  % number of bags of corn taken
var 0..t: trips;                % number of trips taken

var int: obj; % objective: overall profit achieved

%%%%%%%

% add your constraints here

%%%%%%%

solve ::int_search([trips], input_order, indomain_min) maximize obj;


output ["  fox = ", show(fox),
     ";\ngeese = ", show(geese),
     ";\n corn = ", show(corn),
     ";\ntrips = ", show(trips), 
     ";\n  obj = ", show(obj), ";\n"];
