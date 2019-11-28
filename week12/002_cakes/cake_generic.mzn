% The products to produce
enum Products;
% The profit for each unit of each product
array[Products] of int: profit;

% The available resources
enum Resources;
% The capacity of each resource
array[Resources] of int: capacity;

% How much each unit of each product consumes of each resource
array[Products,Resources] of int: consumption;

% Decision variables: how many units to produce of each product
array[Products] of var 0..infinity: produce;

% TODO: constraints
% The overall consumption cannot exceed the capacity of each resource

% TODO: objective function, maximize the overall profit
var int: overall_profit;
solve maximize overall_profit;

output [ "Number of \(p): \(produce[p])\n" | p in Products]++["Profit: \(overall_profit)"];
