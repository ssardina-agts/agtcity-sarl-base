%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TOOLS	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% Concatenate a list of strings
strings_concat([], "").
strings_concat([S|L], CS) :-
	strings_concat(L, CS2),
	string_concat(S, CS2, CS).
	

%get_iso_time(T) :-
%    get_time(X), 
%    format_time(atom(T),'%Y-%m-%d--%H-%M-%S',X,posix).



my_save_db :-
	(agentName(Name) -> true ; Name = default),
	(step(Step) ; Step = "none"), !,
	strings_concat(["kb-", Name, "-", Step, ".pl"], FileName),
	open(FileName, write, F), 
	set_output(F), 
	listing,
	close(F).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
