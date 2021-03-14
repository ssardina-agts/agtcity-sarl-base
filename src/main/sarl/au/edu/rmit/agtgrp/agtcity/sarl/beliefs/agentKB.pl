%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DECISION MAKING REASONING TOOLS!
%
% This is what needs to be developed!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(libtools).

% :- include(percepts_types_KB).
:- include(percepts_holds_KB).
% :- include(percepts_process_KB).
:- include(percepts_examples).

% :- include(actions_base).	    % reasoning of what ACTION the agent needs to do on the game server
% :- include(build_base).		% reasoning for BUILDING complex items
% :- include(charge_base).	    % reasoning for ENERGY and CHARGING management
% :- include(contracts_base).	% reasoning about CONTRACTS
:- include(location_KB).	    % reasoning about LOCATIONS and DISTANCE
% :- include(facility_base).	% reasoning about FACILITIES (e.g., shops, workshops, dumps, etc)
% :- include(jobs_base).		% reasoning about JOBS (auctions, missions, regular)
% :- include(items_base).		% reasoning about ITEMS

:- include(decision_making).

% load_unit_test :- load_test_files([]).