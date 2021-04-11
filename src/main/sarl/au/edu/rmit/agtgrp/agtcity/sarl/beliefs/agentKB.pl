/** <agentKB> - Main file for loading the whole KB

@author Sebastian Sardina

@license GPL
@copyright Sebastian Sardina, Joshua Hansen, Adam Young

	SARL agents can just load this file and the whole KB will be consulted
*/
:- use_module(libtools).

:- include(percepts_holds).  % definitions for holds/x predicates to query percepts
%:- ensure_loaded(percepts_mngt).  % included in percepts_holds for management of percepts

:- include(decision_making).
:- include(location).	    % reasoning about LOCATIONS and DISTANCE
% :- include(actions_base).	    % reasoning of what ACTION the agent needs to do on the game server
% :- include(build_base).		% reasoning for BUILDING complex items
% :- include(charge_base).	    % reasoning for ENERGY and CHARGING management
% :- include(contracts_base).	% reasoning about CONTRACTS
% :- include(facility_base).	% reasoning about FACILITIES (e.g., shops, workshops, dumps, etc)
% :- include(jobs_base).		% reasoning about JOBS (auctions, missions, regular)
% :- include(items_base).		% reasoning about ITEMS


% load_unit_test :- load_test_files([]).