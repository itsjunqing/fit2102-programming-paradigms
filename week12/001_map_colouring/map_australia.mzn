% Map colouring

% Problem description:
% Determine the minimum number of colours needed to 
% assign each state and territory of Australia a colour,
% such that no neighbouring states have the same colour.

% maximum number of colours to use
int: nc = 11;

% mapping each state to a colour
var 1..nc: wa;
var 1..nc: nt;
var 1..nc: sa;
var 1..nc: q;
var 1..nc: nsw;
var 1..nc: vic;
var 1..nc: tas;

% TODO: adjacent states have different colours

% TODO: define objective function

var 1..nc: n_colours;

solve minimize n_colours;

% Solution
output ["WA  = ",show(wa),"\n",
        "NT  = ",show(nt),"\n",
        "SA  = ",show(sa),"\n",
        "Q   = ",show(q),"\n",
        "NSW = ",show(nsw),"\n",
        "VIC = ",show(vic),"\n",
        "TAS = ",show(tas),"\n"
        ];
