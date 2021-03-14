%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TOOLS
% This are just a set of Prolog tools convenient to have
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(libtools, [
		strings_concat/2,
		my_save_db/0,
		round_to_places/3
	]).

%!      string_concat(+Strings:list, ?Result:string) is det.
%
%	String is the concatenations of all strigns in list Strings
%
%       @arg	Strings - List of strings
%       @arg	Result  - The concatenated string
strings_concat([], "").
strings_concat([S|L], CS) :-
	strings_concat(L, CS2),
	string_concat(S, CS2, CS).


%get_iso_time(T) :-
%    get_time(X),
%    format_time(atom(T),'%Y-%m-%d--%H-%M-%S',X,posix).


%!      my_save_db/0 is det.
%
%	Dumps the KB of the agent to a file kb-<agent name>-<step no>.pl
% 	OBS: Not used as we have a dumping mechanism in Prolog Capacity
my_save_db :-
	(agentName(Name) -> true ; Name = default),
	(step(Step) ; Step = "none"), !,
	strings_concat(["kb-", Name, "-", Step, ".pl"], FileName),
	open(FileName, write, F),
	set_output(F),
	listing,
	close(F).

%!      round_to_places(++N, ++D, ?N3) is det.
%
%	Rounds a number to a decimal place
%
%       @arg	N - Number to round
%       @arg	D - Decimal places to round to
%       @arg	N3 - Rounded value
round_to_places(N, D, N3) :-
	round(N * 10^D, N2),
    N3 is N2 / 10^D.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
