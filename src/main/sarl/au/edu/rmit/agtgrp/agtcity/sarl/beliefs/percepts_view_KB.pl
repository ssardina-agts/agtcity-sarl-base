/** <percepts_base> - Management of **PERCEPTION**

A simple holds/3 predicate to extract info from percepts/3 clauses

@author Sebastian Sardina

@license GPL
@copyright Sebastian Sardina, Joshua Hansen, Adam Young
@tbd Maybe use mutexes for DB acccess: http://www.swi-prolog.org/pldoc/man?section=threadsync


	Every time a percept is received via the MW and EISMassim framework, agent will assert percepts/3:

	percepts(eismassim_connectionA9, 20, [.....])

	The set of possible individual percepts is here:

	2018: https://github.com/agentcontest/massim/blob/massim-2018-1.2/docs/eismassim.md
	2017: https://github.com/agentcontest/massim/blob/massim-2017-1.7/docs/eismassim.md

*/
:- include('percepts_types_KB.pl').


%!      holds(+P:term) is nondet.
%       holds(+P:term, ?E:atom) is nondet.
%       holds(+P:term, ?E:atom, ?S:number) is nondet.
%
%	Predicate P is true for entity E at step S.
%	If step is not given, only the last step is retrived/checked
%
%	@arg	P	the term data to check or extract (type has to be given)
%	@arg	E	the entity (e.g., entityA2)  to ask for only
%	@arg	S	the step number to check
%
holds(P) :- holds(step(S), _, _), ! , holds(P, _, S).
holds(P, E) :- holds(step(S), _, _), !, holds(P, E, S).
holds(P, E, S) :- predicate_translate(P, P2), percepts(E, S, X), member(P2, X).

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
%	Predicate P is true for entity E at step S.
%	If step is not given (first two), only the last step is retrived/checked
%	If entity is not given (first), return for all entities aggregated
%
%	@arg	P	the term data to check or extract (type must be ground)
%	@arg	E	the entity (e.g., entityA2)  to ask for only
%	@arg	S	the step number to check
%	@arg	A	a list with a set of all the answers
%
holds_all(P, A) :-
	holds(step(S), _, _), ! ,
	setof(P2, E^P^Percepts^(predicate_translate(P, P2), percepts(E, S, Percepts), member(P2, Percepts)), A).
holds_all(P, E, A) :- holds(step(S), _, _), !, holds_all(P, E, S, A).
holds_all(P, E, S, A) :-
	setof(P2, P^Percepts^(predicate_translate(P, P2), percepts(E, S, Percepts), member(P2, Percepts)), A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%