%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tools to manipulate percepts from Agents in City Game
%
% Author: Sebastian Sardina 2017-2018 (ssardina@gmail.com) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic 
	id/1, 			% id
	percepts/3,		% main percept data
	percepts_nonself/2, 	% Stores percepts of others
	percepts_self/3, 	% Stores percepts about myself
	step/1, 		% Stores current simulator step number
	agentName/1,		% Stores the name of the agent who owns the KB
	entity/1.		% Stores names of connection entities controlled

:- dynamic storage/6, 
	dump/3,
	chargingStation/4,
	workshop/3,
	resourceNode/4.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Defines the TYPE of each single percept that may come in as json data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
percept_simStart_type(simStart).
percept_simStart_type(id(_)).		% problem with arguments in capital treated as vars
percept_simStart_type(map(_)).		% problem with arguments in capital treated as vars
percept_simStart_type(seedCapital(_)).
percept_simStart_type(steps(_)).
percept_simStart_type(team(_)).		% problem with arguments in capital treated as vars
percept_simStart_type(item(_, _, _, _)).
percept_simStart_type(proximity(_)).
percept_simStart_type(cellSize(_)).
percept_simStart_type(maxLat(_)).
percept_simStart_type(minLat(_)).
percept_simStart_type(centerLat(_)).
percept_simStart_type(maxLon(_)).
percept_simStart_type(minLon(_)).
percept_simStart_type(centerLon(_)).


percept_simEnd_type(simEnd).
percept_simEnd_type(ranking(_)).
percept_simEnd_type(score(_)).

percept_self_type(name(_)). 
percept_self_type(role(_, _, _, _, _)).
percept_self_type(actionID(_)).
percept_self_type(timestamp(_)).
percept_self_type(deadline(_)).
%percept_self_type(step(_)).
percept_self_type(charge(_)).
percept_self_type(load(_)).
percept_self_type(lat(_)).
percept_self_type(lon(_)).
percept_self_type(routeLength(_)).
%percept_self_type(money(_)).
percept_self_type(facility(_)).
percept_self_type(lastAction(_)).
percept_self_type(lastActionParams(_)).
percept_self_type(lastActionResult(_)).
percept_self_type(hasItem(_, _)).
percept_self_type(route(_)).


percept_nonself_type(workshop(_, _, _)).
percept_nonself_type(dump(_, _, _)).
percept_nonself_type(storage(_, _, _, _, _, _)).
percept_nonself_type(item(_, _, _, _)).
percept_nonself_type(money(_)).
percept_nonself_type(entity(_, _, _, _, _)).
percept_nonself_type(chargingStation(_, _, _, _)).
percept_nonself_type(step(_)).
percept_nonself_type(resourceNode(_, _, _, _)).
percept_nonself_type(job(_, _, _, _, _)).
percept_nonself_type(job(_, _, _, _, _, _)).
percept_nonself_type(posted(_, _, _, _, _)).
percept_nonself_type(posted(_, _, _, _, _, _)).
percept_nonself_type(auction(_, _, _, _, _, _, _, _)).
percept_nonself_type(auction(_, _, _, _, _, _, _, _, _)).
percept_nonself_type(mission(_, _, _, _, _, _, _, _)).
percept_nonself_type(mission(_, _, _, _, _, _, _, _, _)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% USEFUL abtractions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
facility(F, storage) :- storage(F, _, _, _, _, _).
facility(F, dump) :- dump(F, _, _).
facility(F, chargingStation) :- chargingStation(F, _, _, _).
facility(F, workshop) :- workshop(F, _, _).
facility(F, resourceNode) :- resourceNode(F, _, _, _).


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
% Several get operations from percepts
% This assumes percepts/3 have been recorded
% Will yield info for all steps recorded
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_entity_data(Entity, Name, Step, Lat, Long, Charge, Load, Money, Facility) :- 
	percepts(Entity, Step, Percepts),
	member(name(Name), Percepts),
	member(lat(Lat), Percepts),
	member(lon(Long), Percepts),
	member(load(Load), Percepts),
	member(charge(Charge), Percepts),
	member(money(Money), Percepts),
	(member(facility(Facility), Percepts) -> true ; Facility = none).
get_entity_name(Entity, Name) :-
	entity(Entity),
	once(percepts(Entity, _, Percepts)),
	member(name(Name), Percepts).
get_player_role(Entity, Role, Speed, Load, Charge, Tools) :- % role(motorcycle, 4, 300, 350, [tool1, tool5])
	entity(Entity),
	once(percepts(Entity, _, Percepts)),
	member(role(Role, Speed, Load, Charge, Tools), Percepts).
get_entity_loc(Entity, Step, Lat, Long) :-
	percepts(Entity, Step, Percepts), !,
	member(lat(Lat), Percepts),
	member(lon(Long), Percepts).
get_entity_charge(Entity, Step, Charge) :-
	percepts(Entity, Step, Percepts), !,
	member(charge(Charge), Percepts).
get_entity_routeLength(Entity, Step, Length) :-
	percepts(Entity, Step, Percepts), !,
	member(routeLength(Length), Percepts).
get_entity_routeLength(Entity, Length) :-       % return last route length
    get_entity_routeLength(Entity, _, Length), !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get operations from the last percept recorded
% Using once(G) will yield just the first success of G
% See once/1 doc: http://www.swi-prolog.org/pldoc/man?predicate=once/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_last_self_percepts(Entity, Step, Percepts) :-
	entity(Entity),
	once(percepts_self(Entity, Step, Percepts)).
get_last_step(Entity, Step) :-
	entity(Entity),
	once(percepts(Entity, Step, _)).
get_entity_last_loc(Entity, Step, Lat, Long) :-
	entity(Entity),
	once(get_entity_loc(Entity, Step, Lat, Long)).
get_entity_last_charge(Entity, Step, Charge) :-
	entity(Entity),
	once(get_entity_charge(Entity, Step, Charge)).


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



% Clean up all stored information about percepts
clean_percepts :-
	(percept_self_type(T) ; percept_simStart_type(T) ; percept_simEnd_type(T)),
	retractall(T),
	fail.
clean_percepts :-
	retractall(percepts_simStart(_)),
	retractall(percepts_self(_, _, _)),
	retractall(percepts_nonself(_, _)),
	retractall(percepts_simEnd(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
