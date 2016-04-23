:- module(predicates, [brachylog_behead/2,
					   brachylog_enumerate/2,
					   brachylog_head/2,
					   brachylog_length/2,
					   brachylog_reverse/2,
					   brachylog_plus/2,
					   brachylog_minus/2,
					   brachylog_greater/2,
					   brachylog_less/2,
					   brachylog_modulo/2]).
					   
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
BRACHYLOG_ENUMERATE
*/
brachylog_enumerate('string':L,'string':M) :-
	nth0(_,L,M).
	
brachylog_enumerate(['integer':SignInf:Inf,'integer':SignSup:Sup],'integer':SignI:I) :-
	integer_value('integer':SignInf:Inf,InfV),
	integer_value('integer':SignSup:Sup,SupV),
	label([InfV,SupV]),
	IV in InfV..SupV,
	integer_value('integer':SignI:I,IV).
	
brachylog_enumerate(['integer':SignSup:Sup,'integer':SignInf:Inf],'integer':SignI:I) :-
	integer_value('integer':SignInf:Inf,InfV),
	integer_value('integer':SignSup:Sup,SupV),
	label([InfV,SupV]),
	IV in InfV..SupV,
	integer_value('integer':SignI:I,IV).
	
brachylog_enumerate('integer':_:L,'integer':'positive':M) :-
	nth0(_,L,M).
	
brachylog_enumerate('float':_:L,'float':'positive':M) :-
	nth0(_,L,M),
	M \= '.'.
	
brachylog_enumerate(L,M) :-
	L \= ['integer':SignI:I,'integer':SignJ:J],
	nth0(_,L,M).

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
BRACHYLOG_REVERSE
*/
brachylog_reverse('string':S,'string':R) :-
	reverse(S,R).
	
brachylog_reverse(List,R) :-
	reverse(List,R).
	
brachylog_reverse('integer':Sign:I,'integer':Sign:R) :-
	reverse(I,R).
	
brachylog_reverse('float':Sign:I,'float':Sign:R) :-
	reverse(I,R).
	
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
	
/*
BRACHYLOG_MINUS
*/
brachylog_minus('integer':Sign:I,'integer':NewSign:I) :-
	(
		I = [0],
		NewSign = 'positive'
		;
		Sign = 'positive',
		NewSign = 'negative'
		;
		Sign = 'negative',
		NewSign = 'positive'
	).
	
brachylog_minus([],'integer':'positive':[0]).
brachylog_minus(['integer':Sign1:I1,'integer':Sign2:I2],Sum) :-
	S #= E - F,
	integer_value('integer':Sign1:I1,E),
	integer_value('integer':Sign2:I2,F),
	integer_value(Sum,S),
	label([S]).
	
/*
BRACHYLOG_LESS
*/
brachylog_less('integer':Sign1:I1,'integer':Sign2:I2) :-
	E1 #< E2,
	integer_value('integer':Sign1:I1,E1),
	integer_value('integer':Sign2:I2,E2).
	
/*
BRACHYLOG_GREATER
*/
brachylog_greater('integer':Sign1:I1,'integer':Sign2:I2) :-
	E1 #> E2,
	integer_value('integer':Sign1:I1,E1),
	integer_value('integer':Sign2:I2,E2).
	
/*
BRACHYLOG_MODULO
*/
brachylog_modulo(['integer':Sign1:I1,'integer':Sign2:I2],'integer':SignR:R) :-
	Rem #= E1 mod E2,
	integer_value('integer':Sign1:I1,E1),
	integer_value('integer':Sign2:I2,E2),
	integer_value('integer':SignR:R,Rem),
	label([Rem]).