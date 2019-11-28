% Open Shop Scheduling

/*
 Problem description:

 We need to complete n jobs using m machines. Each job needs
 processing on each machine for a given time. The order in which
 the different machines are used for a job does not matter.
 The same job cannot be processed at two machines at once,
 and each machine can only process one job at a time.

 The objective is to minimise the makespan, i.e., the time by
 which all jobs are finished.
*/

% Parameters: duration[i,j] is the time it takes to process
% job j on machine i.
array[int,int] of int: duration;

int: NMachines = card(index_set_1of2(duration));
int: NJobs= card(index_set_2of2(duration));

set of int: Machines = 1..NMachines;
set of int: Jobs = 1..NJobs;

% TODO: compute a valid upper bound on the makespan
int: makespanUpperBound = ...;

% Introduce variables for the start time of each
% job on each machine

array[Machines,Jobs] of var 0..makespanUpperBound: start ::add_to_output;

% TODO: introduce a variable for the makespan
% 

var 0..makespanUpperBound: makespan ::add_to_output = ...;

% TODO: introduce constraints that restrict the jobs
% not to overlap as specified

% Minimize the makespan
solve minimize makespan;

output [show2d(start)];
