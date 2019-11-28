% Baking cakes

/*
 Problem description:
 We have 4kg of flour, 6 bananas, 2kg of sugar, 500g of butter and 500g of cocoa.

  Banana cake:
  250g   self-raising flour
  2      mashed bananas
  75g    sugar
  100g   butter

  Chocolate cake:
  200g   self-raising flour
  75g    cocoa
  150g   sugar
  150g   butter  

 Banana cakes sell for $4.00, chocolate cakes for $4.50
 Determine how many of each sort of cake we should make to maximise profit.
*/

% Variables express how many cakes to bake of each type
var 0..5: b; % b to represent banana
var 0..5: c; % c to represent cocoa 

% set the constraints so that banana and chocolate cake will not use more than the available ingredients
constraint 250*b + 200*c <= 4000;
constraint 2*b + 0*c <= 6;
constraint 75*b + 150*c <= 2000;
constraint 100*b + 150*c <= 500;
constraint 0*b + 75*c <= 500;

% set the formula for the profit, then ask minizinc to get the maximum profit based that fulfill the contraints
var float: profit = 4.0*b + 4.5*c;
solve maximize profit;

% Output solution, not important, ignore
output ["b= ",show(b),", c=",show(c),", profit=",show(profit),"\n"];
