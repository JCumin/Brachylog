:- module(predicates, [brachylog_behead/2,
					   brachylog_head/2,
					   brachylog_length/2]).
					   
:- use_module(library(clpfd)).
:- use_module(utils).
				
/*
BRACHYLOG_BEHEAD
*/				
brachylog_behead('string':[_|T],'string':T).

brachylog_behead([_|T],T).

brachylog_behead('integer':_:[_|T],'integer':_:T).

brachylog_behead('float':_:[_|T],'float':_:T).

/*
BRACHYLOG_HEAD
*/
brachylog_head('string':[H|_],'string':[H]).

brachylog_head([H|_],H).

brachylog_head('integer':_:[H|_],'integer':_:[H]).

brachylog_head('float':_:[H|_],'float':_:[H]).

/*
BRACHYLOG_LENGTH
*/
brachylog_length('string':S,Length) :-
	length(S,Length).

brachylog_length(List,Length) :-
	length(List,Length).
	
brachylog_length('integer':_:I,Length) :-
	length(I,Length).
	
brachylog_length('float':_:F,Length) :-
	length(F,Length).
	
/*
BRACHYLOG_PLUS
*/
brachylog_plus('integer':Sign:I,AbsoluteValue) :-
	integer_value('integer':Sign:I,E),
	I #= abs(E),
	label([I]),
	integer_value(AbsoluteValue,I).
	
brachylog_plus([],'integer':'positive':[0]).
brachylog_plus(['integer':Sign:I|T],Sum) :-
	S #= E + F,
	brachylog_plus(T,'integer':SignZ:Z),
	integer_value('integer':SignZ:Z,F),
	integer_value('integer':Sign:I,E),
	integer_value(Sum,S),
	label([S]).