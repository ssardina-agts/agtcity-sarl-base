/** <percepts_base> - Management of **PERCEPTION**

Tools to assimilate percept information from the game server.

Basically, it processes facts percepts/3, which are asserted every time a 
percept arrives, and produces implicit facts.

@author Sebastian Sardina
@author Joshua Hansen
@author Adam Young

@license GPL
@copyright Sebastian Sardina, Joshua Hansen, Adam Young
@tbd Maybe use mutexes for DB acccess: http://www.swi-prolog.org/pldoc/man?section=threadsync


	Every time a percept is received via the MW and EISMassim framework, agent will assert percepts/3:

	percepts(eismassim_connectionA9, 20, [.....])

	The set of possible individual percepts is here:

	2018: https://github.com/agentcontest/massim/blob/massim-2018-1.2/docs/eismassim.md
	2017: https://github.com/agentcontest/massim/blob/massim-2017-1.7/docs/eismassim.md

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tools to manipulate percepts from Agents in City Game
%
% Author: Sebastian Sardina 2017-2018 (ssardina@gmail.com) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic
	percepts/3,			% main percept data
	percepts_nonself/2, % Stores percepts of others
	step/1, 			% Stores current simulator step number
	agentName/1,		% Stores the name of the agent who owns the KB
	entity/1.			% Stores names of connection entities controlled

% all data from game (static during the game)
:- dynamic
	simStart/0,			% to signal that the simulation has started (and global data has been recorded)
	simEnd/0,
	id/1, 				% id
	map/1,
	seedCapital/1,
	step/1,
	team/1,
	item/4,
	wellType/5,
	proximity/1,
	cellSize/1,
	centerLat/1,
	centerLon/1,
	maxLat/1,
	maxLon/1,
	minLat/1,
	minLon/1,
	upgrade/3,
	chargingStation/4,
	dump/3,
	shop/5,
	storage/6,
	workshop/3.

% ROLES
:- dynamic
	role/11.

% Entity specific data (changes per step)
:- dynamic
	state_entity/3. 	% Stores percepts about myself


% DISCOVERIES
:- dynamic
	entity/5,
	well/6,
	resourceNode/4.




:- include('percepts_types_KB.pl').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% USEFUL abtractions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
facility(F, storage) :- storage(F, _, _, _, _, _).
facility(F, dump) :- dump(F, _, _).
facility(F, chargingStation) :- chargingStation(F, _, _, _).
facility(F, workshop) :- workshop(F, _, _).
facility(F, resourceNode) :- resourceNode(F, _, _, _).
facility(F, shop) :- shop(F, _, _ , _, _).


%!      clean_percepts is det.
%
%	Retracts ALL implicit information form percepts asserted before (in predicates percepts_xxx/3)  
clean_percepts :-
	percept_type(P, _), 
	retractall(P),
	fail.
clean_percepts.

% clean_percepts :-
% 	retractall(percepts_simStart(_)),
% 	retractall(percepts_self(_, _, _)),
% 	retractall(percepts_nonself(_, _)),
% 	retractall(percepts_simEnd(_)).


% Extract all percepts of a given Type from AllPercepts into PerceptsOfType 
extract_percepts_by_type(Percepts, Type, PerceptsOfType) :-
	setof(Percept, Percept^(member(Percept, Percepts), percept_type(Percept, Type)), PerceptsOfType).


%!      process_percepts(++Percepts:list) is det.
%
%	Top-level processing predicate: process all the percepts in a list of percepts
%
%	@arg	Percepts	list of single percepts
%
process_percepts(Percepts) :-
	extract_percepts_by_type(Percepts, game, GamePercepts),
	process_percepts(_Player, Percepts, game) :-
		\+ simStart ->  	% no sim-start message processed before
			(extract_percepts(Percepts, game, GamePercepts),
			 foreach(member(Percept, GamePercepts), asserta(Percept))) ; true.

	foreach(member(Percept, Percepts), process_percept(Percept)).

%!      process_percepts_linear(++Percepts:list) is det.
%
%	Top-level processing predicate: process all the percepts in a list of percepts
%
%	@arg	Percepts	list of single percepts
%
process_percepts_linear(Percepts) :-
		foreach(member(Percept, Percepts), process_percept(Percept)).

process_percept(Percept) :- percept_type(Percept, T), process_percept(T, Percept).
process_percept(Percept) :- format('ERROR: percept has no type: "~w"\n', [Percept]).

% STATIC info: does not change during a game
% TODO: too inefficient, too many checks for the existence. We need to do it once and skip the game processing
process_percept(game, Percept) :- \+ Percept, assert(Percept).
process_percept(role, Percept) :- \+ Percept, assert(Percept).


process_percept(_, _).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% percepts(Entity, Step, Percepts): 
%	this is asserted when the Entity receives the list of Percepts at step Step
%	this is the most low-level information recorded; Percepts has everything sensed!
%
% By processing this predicate, we can keep more focused predicates:
%
% percepts_self(Entity, Step, Percepts)
%	this stores all the Percepts at Step that apply only to Entity 
% percepts_nonself(Step, NonSelfPercepts): 
%	this is the information that applies to everyone
% percepts_simStart(SimStartPercepts)
%	this is the information in the sim-start message
% percepts_simEnd(SimEndPercepts)
%	this is the information in the sim-end message
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extract each percept type from a general percept
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extract_simStart_percepts(Percepts, SimStartPercepts) :-
	findall(Percept, (member(Percept, Percepts), percept_simStart_type(Percept)), SimStartPercepts).
extract_simEnd_percepts(Percepts, SimEndPercepts) :-
	findall(Percept, (member(Percept, Percepts), percept_simEnd_type(Percept)), SimEndPercepts).
extract_requestAction_percepts(Percepts, ReqActionPercepts) :-
	findall(Percept, (member(Percept, Percepts), \+ percept_simEnd_type(Percept), \+ percept_simStart_type(Percept)), ReqActionPercepts).
extract_self_percepts(Percepts, SelfPercepts) :-
	findall(Percept, (member(Percept, Percepts), percept_self_type(Percept)), SelfPercepts).
extract_nonself_percepts(Percepts, NonSelfPercepts) :-
	findall(Percept, (member(Percept, Percepts), percept_nonself_type(Percept)), NonSelfPercepts).


assert_percepts(Entity, Step, Percepts) :-
	foreach(member(Percept, Percepts), asserta(percept_term(Entity, Step, Percept))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process the last percept stored for Entity by extracting and asserting each of its 4 component types:
%
% percepts_simStart(SimStartPercepts)
% percepts_self(Entity, Step, SelfPercepts): data for entity itself
% percepts_nonself(Step, NonSelfPercepts): data that applies for everyone
% percepts_simEnd(SimEndPercepts)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_last_percepts(Entity) :-
	percepts(Entity, _, Percepts), !, % get the last percepts/3 asserted in the DB
	process_percepts(Entity, Percepts).

% Transform list of percepts into different local predicates
process_percepts(Entity, Percepts) :-
	process_simStart_percepts(Percepts),		% produces percepts_simStart/1 and one per data
	process_self_percepts(Entity, Percepts),	% produces percepts_self/3
	process_nonself_percepts(Percepts),		% produces percepts_nonself/2
	process_simEnd_percepts(Percepts).		% produces percepts_simEnd/1 and one per data


%% This is done once, because then id/1 will be in the database
%% 	All the entitys/agents receive the same sim-star data, except their roles (which we treat as self percept)
process_simStart_percepts(Percepts) :-	% simstart percepts
	\+ id(_), !, 			% not processed before (this is just 1 time message)
	member(simStart, Percepts),
	extract_simStart_percepts(Percepts, SimStartPercepts),
	foreach(member(Percept, SimStartPercepts), asserta(Percept)),
	asserta(percepts_simStart(SimStartPercepts)).
process_simStart_percepts(_).

process_self_percepts(Entity, Percepts) :-	% self percepts from request action
	member(step(Step), Percepts),
	\+ percepts_self(Entity, Step, _), !,
	extract_self_percepts(Percepts, SelfPercepts),
	asserta(percepts_self(Entity, Step, SelfPercepts)).
process_self_percepts(_, _).

process_nonself_percepts(Percepts) :-	% non-self percepts from request action
	member(step(Step), Percepts),
	\+ percepts_nonself(Step, _), !,
	extract_nonself_percepts(Percepts, NonSelfPercepts),
	foreach(percept_nonself_type(X), retractall(X)),
	foreach(member(Percept, NonSelfPercepts), asserta(Percept)),
	asserta(percepts_nonself(Step, NonSelfPercepts)).
process_nonself_percepts(_).

process_simEnd_percepts(Percepts) :-	% simend percepts
	member(simEnd, Percepts), !,
	extract_simEnd_percepts(Percepts, SimEndPercepts),
	foreach(member(Percept, SimEndPercepts), asserta(Percept)),
	asserta(percepts_simEnd(SimEndPercepts)).
process_simEnd_percepts( _).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
