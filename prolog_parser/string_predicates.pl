:- module(string_predicates, [brachylog_string_lowercase/2,
                              brachylog_string_uppercase/2]).
                       
:- use_module(library(clpfd)).
:- use_module(utils).
                
/*
BRACHYLOG_STRING_LOWERCASE
*/
brachylog_string_lowercase('string':Ls0,'string':Ls) :-
    maplist(downcase_atom, Ls0, Ls).

/*
BRACHYLOG_STRING_UPPERCASE
*/
brachylog_string_uppercase('string':Ls0,'string':Ls) :-
    maplist(upcase_atom, Ls0, Ls).
