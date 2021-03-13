%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tools to manipulate percepts from Agents in City Game
%
% Authors: 
%	Sebastian Sardina 2017-2018 (ssardina@gmail.com) 
%	Joshua Hansen
%
%  TODO; maybe use mutexes for DB acccess: http://www.swi-prolog.org/pldoc/man?section=threadsync
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(percepts, [
			% Processing tools
			process_last_percepts/1,
                        clean_percepts/0,
                        clean_percepts_before/2,
                        process_last_action/1,
                        % Getters
                        get_player_data/2,
                        query/1,
                        get_player_role/2,
                        get_player_item_qty/4,
                        player/1,
                        eisName_username/2,
                        % DB management
                        load_test_percept/0
                        ]).


:- module_transparent(load_test_percept/0).
:- module_transparent(assert_percepts/1).
:- module_transparent(assert_player/1).
:- module_transparent(clean_percepts/0).
:- module_transparent(clean_percepts_before/2).
:- module_transparent(remove_player/1).
:- module_transparent(process_last_percepts/1).

:- include(percepts_base).
