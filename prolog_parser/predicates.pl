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
brachylog_behead('integer':0,'integer':0).
brachylog_behead('integer':I,'integer':J) :-
	H #\= 0,
	integer_value('integer':Sign:[H|T],I),
	integer_value('integer':Sign:T,J).
brachylog_behead([_|T],T).


/*
BRACHYLOG_ENUMERATE
*/
brachylog_enumerate('string':L,'string':M) :-
	nth0(_,L,M).
brachylog_enumerate(['integer':Inf,'integer':Sup],'integer':I) :-
	Sup #>= Inf,
	label([Sup,Inf]),
	between(Inf,Sup,I).
brachylog_enumerate(['integer':Sup,'integer':Inf],'integer':I) :-
	Sup #> Inf,
	label([Sup,Inf]),
	between(Inf,Sup,N),
	I #= Inf + Sup - N.
brachylog_enumerate('integer':I,'integer':J) :-
	integer_value('integer':_:L,I),
	nth0(_,L,M),
	integer_value('integer':'positive':M,J).
brachylog_enumerate(L,M) :-
	L \= ['integer':_,'integer':_],
	nth0(_,L,M).


/*
BRACHYLOG_HEAD
*/
brachylog_head('string':[H|_],'string':[H]).
brachylog_head('integer':0,'integer':0).
brachylog_head('integer':I,'integer':J) :-
	J #\= 0,
	integer_value('integer':_:[J|_],I).
brachylog_head([H|_],H).

/*
BRACHYLOG_LENGTH
*/
brachylog_length('string':S,Length) :-
	length(S,Length).
brachylog_length('integer':I,Length) :-
	integer_value('integer':_:L,I),
	length(L,Length).
brachylog_length(List,Length) :-
	length(List,Length).
	
	
/*
BRACHYLOG_REVERSE
*/
brachylog_reverse('string':S,'string':R) :-
	reverse(S,R).
brachylog_reverse('integer':I,'integer':R) :-
	integer_value('integer':Sign:L,I),
	reverse(L,M),
	integer_value('integer':Sign:M,R).
brachylog_reverse(List,R) :-
	reverse(List,R).
	
/*
BRACHYLOG_WRITE
*/
brachylog_write([List,'string':F],List) :-
	is_list(List),
	atom_string(F,Format),
	format(Format,List).
brachylog_write([X,'string':F],X) :-
	\+ (is_list(X)),
	atom_string(F,Format),
	format(Format,[X]).
brachylog_write(X,X) :-
	write(X).
	

/*
BRACHYLOG_PLUS
*/
brachylog_plus('integer':I,'integer':AbsoluteValue) :-
	AbsoluteValue #= abs(I),
	label([I,AbsoluteValue]).
brachylog_plus(L,'integer':Sum) :-
	brachylog_plus_(L,M,'integer':Sum),
	label([Sum|M]).
	
brachylog_plus_([],[],'integer':0).
brachylog_plus_(['integer':I|T],[I|T2],'integer':Sum) :-
	brachylog_plus_(T,T2,'integer':F),
	Sum #= I + F.
	
/*
BRACHYLOG_MINUS
*/
brachylog_minus('integer':I,'integer':J) :-
	J #= -I,
	label([I,J]).
brachylog_minus([],'integer':[0]).
brachylog_minus(['integer':I1,'integer':I2],'integer':Sum) :-
	Sum #= I1 - I2,
	label([Sum,I1,I2]).
	
/*
BRACHYLOG_LESS
*/
brachylog_less('integer':I1,'integer':I2) :-
	I1 #< I2.
	
/*
BRACHYLOG_GREATER
*/
brachylog_greater('integer':I1,'integer':I2) :-
	I1 #> I2.
	
/*
BRACHYLOG_LESSEQUAL
*/
brachylog_lessequal('integer':I1,'integer':I2) :-
	I1 #=< I2.
		
/*
BRACHYLOG_GREATEREQUAL
*/
brachylog_greaterequal('integer':I1,'integer':I2) :-
	I1 #>= I2.
	
/*
BRACHYLOG_MODULO
*/
brachylog_modulo(['integer':I1,'integer':I2],'integer':Rem) :-
	Rem #= I1 mod I2,
	label([I1,I2,Rem]).