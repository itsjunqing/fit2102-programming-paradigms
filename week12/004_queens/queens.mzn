% The number of queens
int: n;

% The variable q[i] represents the column of queen i
array[1..n] of var 1..n: q;
include "alldifferent.mzn";

% constraint forall (i, j in 1..n where i != j) (q[i] != q[j]);
constraint alldifferent(q);

% constraint forall (i, j in 1..n where i != j) (
%   i + q[i] != j + q[j]
% );
constraint alldifferent([i + q[i] | i in 1..n]);
constraint alldifferent([i - q[i] | i in 1..n]);
% constraint forall (i, j in 1..n where i != j) (
%   i - q[i] != j - q[j]
% );