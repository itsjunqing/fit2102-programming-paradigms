int: n_nurses; % The number of available nurses
int: sched_period; % The number of days in a scheduling period

set of int: nurses = 1..n_nurses;

set of int: period = 1..sched_period;
enum SHIFT = { DAY, EVE, NGT, OFF };

set of SHIFT: WORKING_SHIFT = { DAY, EVE, NGT };

% Input data: required coverage for each type of 
array [period, WORKING_SHIFT] of 0..n_nurses: nurses_coverage;

% Decision variables

% For each nurse and day, determine the shift type
array[nurses, period] of var SHIFT: nurses_schedule;

% Constraints

% For each day, the required shift types are covered:
% the sum of nurses working shift type t on day d must be
% at least nurses_coverage[d,t]


% Pattern rules:
% 1) max 3 consecutive night shifts
% 2) no day shift after a night shift
% 3) min 2 consecutive day shifts
% 4) min 2 consecutive evening shifts
% 5) max 6 working days per 7 day stretch

% Hint: create one DFA for rules 1)-4), and one for 5)

% Objective function: maximize number of nurses who are not required
% (i.e., who have only OFF shifts)

var int: objective;

solve maximize objective;

output ["Objective: \(objective)\nSchedule:\n"]++[show(nurses_schedule[n,..])++"\n" | n in nurses];