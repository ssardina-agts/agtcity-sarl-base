:- ensure_loaded(percepts_holds_KB).

%!      shops(?Entity:atom, ?Name:atom, ?Loc:term, ?Restock:number, ?Items:list, -A: list) is nondet.
%       shop(?Entity:atom, ?Name:atom, ?Loc:term, ?Restock:number, ?Items:list) is nondet.
%
%       shops(?Name:atom, ?Loc:term, ?Restock:number, ?Items:list, -A: list) is nondet.
%       shop(?Name:atom, ?Loc:term, ?Restock:number, ?Items:list) is nondet.
%
%	Checks or retrives the existing shops as has been perceived by entity E
%
%	The location is combined into a term geo(Lat, Lon)
%   If the Entity is missing (last 2 queries), the data from one arbitrary entity will be used
%       this is useful as every entity perceives the same set of shops so it can
%       avoid redundant answers (i.e., getting the same shop many times)
shops(E, Name, geo(Lat, Lon), Restock, Items, A) :-
    holds_all(shop(Name, Lat, Lon, Restock, Items), E, A).
shops(Name, geo(Lat, Lon), Restock, Items, A) :-
    entity(E), !,
    holds_all(shop(Name, Lat, Lon, Restock, Items), E, A).

shop(E, Name, geo(Lat, Lon), Restock, Items) :-
    holds(shop(Name, Lat, Lon, Restock, Items), E).
shop(Name, geo(Lat, Lon), Restock, Items) :-
    entity(E), !,
    holds(shop(Name, Lat, Lon, Restock, Items), E).

