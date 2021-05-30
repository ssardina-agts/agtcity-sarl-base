/** <percepts_mngt> - Management of percepts

@author Sebastian Sardina
@author Joshua Hansen
@author Adam Young

@license GPL
@copyright Sebastian Sardina, Joshua Hansen, Adam Young
@tbd Maybe use mutexes for DB acccess: http://www.swi-prolog.org/pldoc/man?section=threadsync

	Percepts come as JSON data from server, but they are converted into a list of Prolog
	terms by the EIS. Each term is an individual percept.
	The MW (via the EISMassim framework) will gather/provide that list directly.

	The set of possible individual percepts_sensed is here:

	2018-RMIT: https://github.com/ssardina-agts/agtcity-server/blob/master/docs/eismassim.md
	2018: https://github.com/agentcontest/massim/blob/massim-2018-1.2/docs/eismassim.md
	2017: https://github.com/agentcontest/massim/blob/massim-2017-1.7/docs/eismassim.md
*/
:- dynamic
	entity_registered/1,	% an entity has been registered for perception
	percepts_sensed/3.		% records list of percepts for an entity at a step


%!      add_percepts(++E: atom, ++P: list) is det.
%
%		Asserts a percets_sensed/3 clause to record the sensing of an entity E
add_percepts(E, P) :-
	member(step(S), P),
	asserta(percepts_sensed(E, S, P)).


%!      entity(?E: atom) is nondet.
%
% 		E is an entity that has been registered for tracking or
%		for which there is some percept available
%		Optimized yield each entity once
entity(E) :- entities(Es), member(E, Es).

%!      entities(?Es: list) is det.
%
% 		Es is a list of all the entities known (registered or that have sensed)
entities(Es) :- setof(E, entity_registered(E), Es), !.
entities(Es) :- setof(E, A^B^percepts_sensed(E, A, B), Es).

%!      entity_name(?E: atom, ?N: atom) is nondet.
%
% 		N is the agent name (e.g., agentA4) for entity E (e.g., entityA4)
%		The entity is known at connection time, but the agent name is only known
%		via a sensing percept (percept name(_))
entity_name(E, N) :-
	entities(Es), !,
	member(E, Es),
	once(percepts_sensed(E, _, P)),
	member(name(N), P).

%!      register_entity(?E: atom) is ndet.
%	    unregister_entity(?E: atom) is ndet.
%
% 		Register and unregister an entity for tracking of percepts
register_entity(E) :- asserta(entity_registered(E)).
unregister_entity(E) :- retractall(entity_registered(E)).


%!      step(?E: atom, ?N: number) is ndet.
%       step(?N: number) is det.
%
% 	step/1: S is a last step number for which there is some percept
%	step/2: last step number for each entity E
step(S) :- percepts_sensed(_, S, _), !.
step(E, S) :- entity(E), once(percepts_sensed(E, S, _)).



%!      percept(?Type:atom) is nondet.
%
%	Defines a Type of percept
%
%	@arg	Type	Type of percept (game, self, role, world, discovery)
percept(game).		% Static game data coming from sim-start or sim-end messages (apply to all agents)
percept(self).		% Dynamic data about the sate of the entity that sensed (comes in request-action message), e.g., location
percept(role).		% Static data about the entity sensed about its role and properties (what role, max speed)
percept(world).		% Data about the world under complete information; it missing it is not there (e.g., jobs or massim $)
percept(discovery).	% Data about the world under incomplete information; sensed only on proximity (e.g., resource nodes)

%!      percept(?Percept:term, ?Type:atom) is nondet.
%
%	Defines a Percept of a given Type
%
%   @arg	Percept		A single percept term
%	@arg	Type		Type of percept (self, role, world, discovery)

%% GAME: Static game data coming from sim-start or sim-end messages (apply to all agents)
percept(simStart, game).
percept(id(_), game).		% problem with arguments in capital treated as vars
percept(map(_), game).		% problem with arguments in capital treated as vars
percept(seedCapital(_), game).
percept(steps(_), game).
percept(team(_), game).		% problem with arguments in capital treated as vars
percept(item(_, _, _, _), game).
percept(wellType(_, _, _, _, _), game).	% 2018
percept(proximity(_), game).	% How many decimal places of the lat/long values are compared when checking equality of Locations.
percept(cellSize(_), game).
percept(centerLat(_), game).
percept(centerLon(_), game).
percept(maxLat(_), game).
percept(minLat(_), game).
percept(maxLon(_), game).
percept(minLon(_), game).
percept(upgrade(_, _, _), game).
percept(simEnd, game).	% from sim-end

percept(chargingStation(_, _, _, _), game).
percept(dump(_, _, _), game).
percept(shop(_, _, _, _, _), game).
percept(storage(_, _, _, _, _, _), game).
percept(workshop(_, _, _), game).


% SELF: This is dynamic but complete data of the entity game agent itself coming from request-action message
percept(name(_), self).
percept(role(_, _, _, _, _, _, _, _, _, _, _), self). % 2018 version, from sim-start
percept(actionID(_), self).
percept(timestamp(_), self).
percept(deadline(_), self).
percept(speed(_), self).
percept(load(_), self).
percept(maxLoad(_), self).
percept(charge(_), self).
percept(maxBattery(_), self).
percept(skill(_), self).
percept(vision(_), self).
percept(lat(_), self).
percept(lon(_), self).
percept(routeLength(_), self).
percept(facility(_), self).
percept(lastAction(_), self).
percept(lastActionParams(_), self).
percept(lastActionResult(_), self).
percept(hasItem(_, _), self).
percept(route(_), self).
percept(ranking(_), self).	% coming from sim-end message
percept(score(_), self).	% coming from sim-end message

% ROLE: These are the static specification of each entity role, truck, drone, etc 
% 	role(role, baseSpeed, maxSpeed, baseLoad, maxLoad, baseSkill, maxSkill, baseVision, maxVision, baseBattery, maxBattery)
percept(role(_, _, _, _, _, _, _, _, _, _, _), role).
% percept(role(_), role).
% percept(baseSpeed(_), role).
% percept(maxSpeed(_), role).
% percept(baseLoad(_), role).
% percept(maxLoad(_), role).
% percept(baseSkill(_), role).
% percept(maxSkill(_), role).
% percept(baseVision(_), role).
% percept(maxVision(_), role).
% percept(baseBattery(_), role).
% percept(maxBattery(_), role).


% WORLD: Dynamic but complete world data coming from request-action
% This data changes every step but is always given. So can be fully updated
percept(step(_), world).
percept(massium(_), world).
percept(job(_, _, _, _, _, _), world).
percept(auction(_, _, _, _, _, _, _, _, _), world).
percept(mission(_, _, _, _, _, _, _, _, _), world).
percept(entity(_, _, _, _, _), world).	% for entities in the same team (always observed)

% DISCOVERY: Dynamic data but not complete at every step
%	if it is not seen in a step, it does NOT mean it is false
%	so agents should remember the discoveries along steps
percept(entity(_, _, _, _, _), discovery).	% for entities in other teams (observed only in proximity)
percept(resourceNode(_, _, _, _), discovery).
percept(well(_, _, _, _, _, _), discovery).
