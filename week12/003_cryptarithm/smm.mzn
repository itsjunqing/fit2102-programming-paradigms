% var 0..9: S;
% var 0..9: E;
% var 0..9: N;
% var 0..9: D;
% var 0..9: M;
% var 0..9: O;
% var 0..9: R;
% var 0..9: Y;

% SEND + MORE = MONEY


% SEND + MOST = MONEY
% UNIQUE CHARACTERS = S,E,N,D,M,O,T,Y

var 1..9: S;
var 0..9: E;
var 0..9: N;
var 0..9: D;
var 1..9: M;
var 0..9: O;
var 0..9: T;
var 0..9: Y;

constraint S!=E /\ S!=N /\ S!=D /\ S!=M /\ S!=O /\ S!=T /\ S!=Y 
                /\ E!=N /\ E!=D /\ E!=M /\ E!=O /\ E!=T /\ E!=Y 
                   		  /\ N!=D /\ N!=M /\ N!=O /\ N!=T /\ N!=Y 
                      			  	    /\ D!=M /\ D!=O /\ D!=T /\ D!=Y 
                  								       	/\ M!=O /\ M!=T /\ M!=Y 
                  								       			 	  /\ O!=T /\ O!=Y 
                  																        /\ T!=Y;

constraint 1000 * S + 100 * E + 10 * N + D
         + 1000 * M + 100 * O + 10 * S + T
         = 10000 * M + 1000 * O + 100 * N + 10 * E + Y;

var int: money;

constraint money = 10000 * M + 1000 * O + 100 * N + 10 * E + Y;

solve maximize money;