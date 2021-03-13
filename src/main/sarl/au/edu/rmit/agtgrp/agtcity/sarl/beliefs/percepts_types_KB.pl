/** <percepts_base> - Management of **PERCEPTION**

Defines the types of percepts available

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


/*
	Percepts come as JSON data from server, but they are converted into a list of Prolog
	terms by the EIS. Each term is an individual percept. 
	The MW will gather/provide that list directly.

	The set of possible individual percepts is here:
		2018: https://github.com/agentcontest/massim/blob/massim-2018-1.2/docs/eismassim.md
		2017: https://github.com/agentcontest/massim/blob/massim-2017-1.7/docs/eismassim.md

We classify each individual percept on the following TYPES:

	self 	: coming at every step that are about the player, such as charging level and location
	role	: static specification of each entity role, truck, drone, etc
	world 	: coming at every step that are about non-player information, such as location of shops
			about complete information: if it is not there, it is FALSE
	discovery : coming at every step that are about non-player information but may not be complete
			about incomplete information: if it is not there, it may still be TRUE
			so if a player sees it true, it should remember it even if it is not sensed after
 */

%!      percept_type(?Type:atom) is nondet.
%
%	Defines a Type of percept
%
%	@arg	Type	Type of percept (game, self, role, world, discovery)
percept_type(game).
percept_type(self).
percept_type(role).
percept_type(world).
percept_type(discovery).

%!      percept_type(?Percept:term, ?Type:atom) is nondet.
%
%	Defines a Percept of a given Type
%
%   @arg	Percept		A single percept term
%	@arg	Type		Type of percept (self, role, world, discovery)

%% GAME: Static game data coming from sim-start or sim-end messages (apply to all agents)
percept_type(simStart, game).
percept_type(id(_), game).		% problem with arguments in capital treated as vars
percept_type(map(_), game).		% problem with arguments in capital treated as vars
percept_type(seedCapital(_), game).
percept_type(steps(_), game).
percept_type(team(_), game).		% problem with arguments in capital treated as vars
percept_type(item(_, _, _, _), game).
percept_type(wellType(_, _, _, _, _), game).	% 2018
percept_type(proximity(_), game).	% How many decimal places of the lat/long values are compared when checking equality of Locations.
percept_type(cellSize(_), game).
percept_type(centerLat(_), game).
percept_type(centerLon(_), game).
percept_type(maxLat(_), game).
percept_type(minLat(_), game).
percept_type(maxLon(_), game).
percept_type(minLon(_), game).
percept_type(simEnd, game).	% from sim-end

percept_type(chargingStation(_, _, _, _), game).
percept_type(dump(_, _, _), game).
percept_type(shop(_, _, _), game).
percept_type(storage(_, _, _, _, _, _), game).
percept_type(workshop(_, _, _), game).


% SELF: This is dynamic but complete data of the entity game agent itself coming from request-action message
percept_type(name(_), self).
percept_type(role(_, _, _, _, _, _, _, _, _, _, _), self). % 2018 version, from sim-start
percept_type(actionID(_), self).
percept_type(timestamp(_), self).
percept_type(deadline(_), self).
percept_type(speed(_), self).
percept_type(load(_), self).
percept_type(maxLoad(_), self).
percept_type(charge(_), self).
percept_type(maxBattery(_), self).
percept_type(skill(_), self).
percept_type(vision(_), self).
percept_type(lat(_), self).
percept_type(lon(_), self).
percept_type(routeLength(_), self).
percept_type(facility(_), self).
percept_type(lastAction(_), self).
percept_type(lastActionParams(_), self).
percept_type(lastActionResult(_), self).
percept_type(hasItem(_, _), self).
percept_type(route(_), self).
percept_type(ranking(_), self).	% coming from sim-end message
percept_type(score(_), self).	% coming from sim-end message

% ROLE: These are the static specification of each entity role, truck, drone, etc 
percept_type(role(_), role).
percept_type(baseSpeed(_), role).
percept_type(topSpeed(_), role).
percept_type(baseLoad(_), role).
percept_type(topLoad(_), role).
percept_type(baseBattery(_), role).
percept_type(topBattery(_), role).
percept_type(baseSkill(_), role).
percept_type(topSkill(_), role).
percept_type(baseVision(_), role).
percept_type(topVision(_), role).
percept_type(tools(_), role).


% WORLD: Dynamic but complete world data coming from request-action
% This data changes every step but is always given. So can be fully updated
percept_type(step(_), world).
percept_type(massium(_), world).
percept_type(job(_, _, _, _, _, _), world).
percept_type(auction(_, _, _, _, _, _, _, _, _), world).
percept_type(mission(_, _, _, _, _, _, _, _, _), world).
percept_type(entity(_, _, _, _, _), world).	% for entities in the same team (always observed)

% DISCOVERY: Dynamic data but not complete at every step
%	if it is not seen in a step, it does NOT mean it is false
%	so agents should remember the discoveries along steps
percept_type(entity(_Name, _, _, _, _), discovery).	% for entities in other teams (observed only in proximity)
percept_type(resourceNode(_Name, _, _, _), discovery).
percept_type(well(_Name, _, _, _, _, _), discovery).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
