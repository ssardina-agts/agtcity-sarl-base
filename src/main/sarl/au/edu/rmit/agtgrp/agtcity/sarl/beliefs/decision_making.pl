%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DECISION MAKING REASONING TOOLS!
%
% This is what needs to be developed!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(library(random)).

%% Chooses a random Destination for Player
choose_destination(E, Destination) :-
	holds_all(facility(_, _), E, L),
	random_member(facility(Destination, _), L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
