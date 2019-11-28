% Stable marriage problem

/*
 Problem description:

 There are n (straight) men and n (straight) women.
 Each man has a ranked list of women and vice versa.
 A LOWER NUMBER IN THE RANK MEANS A HIGHER PREFERENCE!
 We want to find a husband/wife for each woman/man such that all marriages are stable:
  - if m prefers another woman o to his wife w, then o prefers her husband to m
  - if w prefers another man o to her husband h, then o prefers his wife to w
*/

% Parameters

enum Men;
enum Women; 

% rankWomen[i,j] is the rank that woman i assigns to man j
array[Women, Men] of int: rankWomen;
% rankMen[i,j] is the rank that man i assigns to woman j
array[Men, Women] of int: rankMen;

% Variables

% Map each man to his wife
array[Men] of var Women: wife ::add_to_output;
% Map each woman to her husband
array[Women] of var Men: husband ::add_to_output;

% TODO: constraints that link the wife/husband assignments
constraint forall (m in Men) (husband[wife[m]] == m);
constraint forall (w in Women) (wife[husband[w]] == w);

/* TODO: the preference constraints
    for each man m and woman w,
     - if m prefers another woman w to his wife,
       then w prefers her husband to m
*/
constraint forall (m in Men, w in Women) (
  if husband[w] !=m /\ rankWomen[w,m] < rankWomen[w, husband[w]]
  then rankMen[m,w] > rankMen[m, wife[m]]
  endif
);

var int: happiness = sum(m in Men) (rankMen[m, wife[m]]) 
                + sum(w in Women) (rankWomen[w, husband[w]]);
                

solve maximize happiness;
output ["happiness=",show(happiness),"\n"];

bool: flag = true;
float: someNum = 0.123;
