:- module(predicates, [brachylog_behead/2,
					   brachylog_head/2,
					   brachylog_length/2]).
				
/*
BRACHYLOG_BEHEAD
*/				
brachylog_behead('string':[_|T],'string':T).

brachylog_behead([_|T],T).

brachylog_behead('integer':[_|T],'integer':T).

brachylog_behead('float':[_|T],'float':T).

/*
BRACHYLOG_HEAD
*/
brachylog_head('string':[H|_],'string':[H]).

brachylog_head([H|_],[H]).

brachylog_head('integer':[H|_],'integer':[H]).

brachylog_head('float':[H|_],'float':[H]).

/*
BRACHYLOG_LENGTH
*/
brachylog_length('string':S,Length) :-
	length(S,Length).

brachylog_length(List,Length) :-
	length(List,Length).
	
brachylog_length('integer':I,Length) :-
	length(I,Length).
	
brachylog_length('float':F,Length) :-
	length(F,Length).