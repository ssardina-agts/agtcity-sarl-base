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
% 		E is an entity thta has been registered for tracking or 
%		for which there is some percept available
entity(E) :- \+ \+ entity_registered(_), !, entity_registered(E).
entity(E) :- percepts_sensed(E, _, _).

%!      entities(?Es: list) is det.
%
% 		Es is a list of all the entities known
entities(Es) :- setof(E, entity(E), Es).

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


%!      step(?N: number) is ndet.
%
% 	S is a last step number for which there is some percept
step(S) :- percepts_sensed(_, S, _).
