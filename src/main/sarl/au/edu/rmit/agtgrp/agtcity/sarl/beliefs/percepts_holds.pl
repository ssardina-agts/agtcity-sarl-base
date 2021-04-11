/** <percepts_holds_KB> - Definition of holds/3 and variant predicates

Predicate holds/3 and variants allow to query whether a predicate (as a single percept)
is true wrt sensed clauses percepts_sensed(E, S, P): P is a list of percepts sensed by
entity E at step S

@author Sebastian Sardina
@license GPL
@tbd Maybe use mutexes for DB acccess: http://www.swi-prolog.org/pldoc/man?section=threadsync

	Predicates that can be checked via holds/3 are:

		1. Single percepts as coming from the server in perception.
		2. Higher-level predicates translated to single percepts.

	The set of possible individual percepts that can be sensed are:

	2018-RMIT: https://github.com/ssardina-agts/agtcity-server/blob/master/docs/eismassim.md
	2018: https://github.com/agentcontest/massim/blob/massim-2018-1.2/docs/eismassim.md
	2017: https://github.com/agentcontest/massim/blob/massim-2017-1.7/docs/eismassim.md
*/
:- include(percepts_mngt).


%!      holds(+P:term) is nondet.
%       holds(+P:term, ?E:atom) is nondet.
%       holds(+P:term, ?E:atom, ?S:number) is nondet.
%
%	Predicate P is true for entity E at step S.
%	If step is not given (first two), only the last step is retrived/checked.
%	If entity is not given (first), return for all entities aggregated
%
%	Relies on percepts_sensed/3 that stores a list of single percepts per entity per step
%
%	@arg	P	the term data to check or extract (type has to be given)
%	@arg	E	the entity (e.g., entityA2)  to ask for only
%	@arg	S	the step number to check
%
holds(P) :- step(S), ! , holds(P, _, S).	% ignore entity, pick any/all. Use last step.
holds(P, E) :- step(S), !, holds(P, E, S).	% use last step  only

holds(and([P]), E, S) :- !, holds(P, E, S).
holds(and([P|L]), E, S) :- !, holds(P, E, S), holds(and(L), E, S).
holds(or(L), E, S) :- !, member(P, L), holds(P, E, S).
holds(P, E, S) :- predicate_translate(P, P2), percepts_sensed(E, S, X), member(P2, X).


% Role propositions translate to:
% 	role(role, baseSpeed, maxSpeed, baseLoad, maxLoad, baseSkill, maxSkill, baseVision, maxVision, baseBattery, maxBattery)
predicate_translate(role(X), role(X, _, _, _, _, _, _, _, _, _, _)).
predicate_translate(baseSpeed(X), role(_, X, _, _, _, _, _, _, _, _, _)).
predicate_translate(maxSpeed(X), role(_, _, X, _, _, _, _, _, _, _, _)).
predicate_translate(baseLoad(X), role(_, _, _, X, _, _, _, _, _, _, _)).
predicate_translate(maxLoad(X), role(_, _, _, _, X, _, _, _, _, _, _)).
predicate_translate(baseSkill(X), role(_, _, _, _, _, X, _, _, _, _, _)).
predicate_translate(maxSkill(X), role(_, _, _, _, _, _, X, _, _, _, _)).
predicate_translate(baseVision(X), role(_, _, _, _, _, _, _, X, _, _, _)).
predicate_translate(maxVision(X), role(_, _, _, _, _, _, _, _, X, _, _)).
predicate_translate(baseBattery(X), role(_, _, _, _, _, _, _, _, _, X, _)).
predicate_translate(maxBattery(X), role(_, _, _, _, _, _, _, _, _, _, X)).

predicate_translate(facility(F, storage), storage(F, _, _, _, _, _)).
predicate_translate(facility(F, dump), dump(F, _, _)).
predicate_translate(facility(F, chargingStation), chargingStation(F, _, _, _)).
predicate_translate(facility(F, workshop), workshop(F, _, _)).
predicate_translate(facility(F, resourceNode), resourceNode(F, _, _, _)).
predicate_translate(facility(F, shop), shop(F, _, _ , _, _)).

predicate_translate(P, P).	% default



%!      holds_all(+P:term, -A: list) is nondet.
%       holds_all(+P:term, ?E:atom, -A: list) is nondet.
%       holds_all(+P:term, ?E:atom, ?S:number, -A: list) is nondet.
%
%	Returns all the true answers of a query as a list
%	If step is not given (first two), only the last step is retrived/checked
%	If entity is not given (first), return for all entities aggregated
%
%	@arg	P	the term data to check or extract (type must be ground)
%	@arg	E	the entity (e.g., entityA2)  to ask for only
%	@arg	S	the step number to check
%	@arg	A	a list with a set of all the answers
%
holds_all(P, A) :-
	step(S), ! ,
	setof(P, E^P2^Percepts^(predicate_translate(P, P2), percepts_sensed(E, S, Percepts), member(P2, Percepts)), A).
holds_all(P, E, A) :- step(S), !, holds_all(P, E, S, A).
holds_all(P, E, S, A) :-
	setof(P, P2^Percepts^(predicate_translate(P, P2), percepts_sensed(E, S, Percepts), member(P2, Percepts)), A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%