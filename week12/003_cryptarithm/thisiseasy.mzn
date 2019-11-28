var 1..9: T;
var 0..9: H;
var 1..9: I;
var 0..9: S;
var 1..9: E;
var 0..9: A;
var 0..9: Y;

constraint T≠H /\ T≠I /\ T≠S /\ T≠E /\ T≠A /\ T≠Y
               /\ H≠I /\ H≠S /\ H≠E /\ H≠A /\ H≠Y
                      /\ I≠S /\ I≠E /\ I≠A /\ I≠Y
                             /\ S≠E /\ S≠A /\ S≠Y
                                    /\ E≠A /\ E≠Y
                                           /\ A≠Y;

constraint 1000 * T + 100 * H + 10 * I + S
                              + 10 * I + S
         = 1000 * E + 100 * A + 10 * S + Y;

% constraint T + H + I + S
%                               + I + S
%          = E + A + S + Y;
