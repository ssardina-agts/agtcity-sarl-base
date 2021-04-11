:- use_module(libtools).

:- ensure_loaded(percepts_holds_KB).  % definitions for holds/x predicates to query percepts
:- ensure_loaded(percepts_types_KB). % definitions of type of percepts in the game
:- ensure_loaded(percepts_mngt_KB).  % management of percepts (e.g., recording, registering entities)

:- ensure_loaded(location_KB).	    % reasoning about LOCATIONS and DISTANCE
% :- include(actions_base).	    % reasoning of what ACTION the agent needs to do on the game server
% :- include(build_base).		% reasoning for BUILDING complex items
% :- include(charge_base).	    % reasoning for ENERGY and CHARGING management
% :- include(contracts_base).	% reasoning about CONTRACTS
% :- include(facility_base).	% reasoning about FACILITIES (e.g., shops, workshops, dumps, etc)
% :- include(jobs_base).		% reasoning about JOBS (auctions, missions, regular)
% :- include(items_base).		% reasoning about ITEMS

:- include(decision_making).

% load_unit_test :- load_test_files([]).