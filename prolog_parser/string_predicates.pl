:- module(string_predicates, [brachylog_string_lowercase/2,
                              brachylog_string_uppercase/2]).
                       
:- use_module(library(clpfd)).
:- use_module(utils).
                
/*
BRACHYLOG_STRING_LOWERCASE
*/
brachylog_string_lowercase('string':[],'string':[]).
brachylog_string_lowercase('string':[H|T],'string':[H2|T2]) :-
    downcase_atom(H,H2),
    brachylog_string_lowercase('string':T,'string':T2).
    
/*
BRACHYLOG_STRING_UPPERCASE
*/
brachylog_string_uppercase('string':[],'string':[]).
brachylog_string_uppercase('string':[H|T],'string':[H2|T2]) :-
    upcase_atom(H,H2),
    brachylog_string_uppercase('string':T,'string':T2).