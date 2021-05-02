% UNCOMMENT ONLY TO DEVELOP&EDIT IN VSCODE WITHOUT MISSING ERRORS
% COMMENT OUT WHEN RUNNING THE SYSTEM OR WILL GIVE ERROR
%:- ensure_loaded(percepts_holds).  

%!      shops(?Entity:atom, -Shops: list) is nondet.
%       shops(-Shops: list) is det.
%
%       shop(?Entity:atom, ?Name:atom, ?Loc:term, ?Restock:number, ?Items:list) is nondet.
%       shop(?Name:atom, ?Loc:term, ?Restock:number, ?Items:list) is nondet.
%
%	Checks or retrives the existing shops as has been perceived by entity E
%
%	The location is combined into a term geo(Lat, Lon)
%   If the Entity is missing, the data from one arbitrary entity will be used
%       this is useful as every entity perceives the same set of shops so it can
%       avoid redundant answers (i.e., getting the same shop many times)
shops(E, Shops) :-
    holds_all(shop(_, _, _, _, _), E, Shops).
shops(Shops) :-
    entity(E), !,
    holds_all(shop(_, _, _, _, _), E, Shops).

shop(E, Name, geo(Lat, Lon), Restock, Items) :-
    holds(shop(Name, Lat, Lon, Restock, Items), E).
shop(Name, geo(Lat, Lon), Restock, Items) :-
    entity(E), !,
    holds(shop(Name, Lat, Lon, Restock, Items), E).


