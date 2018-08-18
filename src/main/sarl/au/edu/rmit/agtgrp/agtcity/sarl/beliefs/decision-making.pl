%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DECISION MAKING REASONING TOOLS!
%
% This is what needs to be developed!	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Chooses a random Destination for Player
choose_destination(_Player, Destination) :-
	findall(F, facility(F, _),L), 
	random_permutation(L, L2), 
	member(Destination, L2).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
