:- module(predicates, [brachylog_behead/2,
					   brachylog_enumerate/2,
					   brachylog_findall/2,
					   brachylog_head/2,
					   brachylog_length/2,
					   brachylog_reverse/2,
					   brachylog_write/2,
					   brachylog_call_predicate/2,
					   brachylog_plus/2,
					   brachylog_minus/2,
					   brachylog_greater/2,
					   brachylog_less/2,
					   brachylog_lessequal/2,
					   brachylog_greaterequal/2,
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
BRACHYLOG_FINDALL
*/
brachylog_findall(X,Y) :-
	append(X,['ignore_calling_predicate'],X2),
	findall(A,brachylog_call_predicate(X2,A),Y).

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
	atomic_list_concat(F,Format),
	brachylog_prolog_variable(List,PrologList),
	format(Format,PrologList).
brachylog_write(Args,S) :-
	is_list(Args),
	reverse(Args,['string':F|R]),
	reverse(R,S),
	brachylog_prolog_variable(S,PrologS),
	atomic_list_concat(F,Format),
	format(Format,PrologS).
brachylog_write('string':S,'string':S) :-
	atomic_list_concat(S,X),
	write(X).
brachylog_write('integer':I,'integer':I) :-
	write(I).
brachylog_write('float':F, 'float':F) :-
	write(F).
brachylog_write(List,List) :-
	is_list(List),
	\+ (reverse(List,['string':_|_])),
	brachylog_prolog_variable(List,PrologList),
	write(PrologList).
	
/*
BRACHYLOG_CALL_PREDICATE
*/
brachylog_call_predicate(X,Output) :-
	(
		reverse(X,[CallingPredName,P|RArgs])
		;
		X = [CallingPredName],
		P = 'no_args',
		RArgs = []
	),
	reverse(RArgs,Args),
	(
		P = 'integer':I,
		(
			I = 0,
			PredName = 'brachylog_main'
			;
			I #> 0,
			atomic_list_concat(['brachylog_predicate_',I],PredName)
		),
		RealArgs = Args
		;
		P \= 'integer':_,
		atom(P),
		atom_concat('brachylog_',_,P),
		PredName = P,
		RealArgs = Args
		;
		P \= 'integer':_,
		\+ (atom(P), atom_concat('brachylog_',_,P)),
		PredName = CallingPredName,
		(
			P = 'no_args',
			RealArgs = []
			;
			P \= 'no_args',
			reverse([P|RArgs],RealArgs)
		)
	),
	(
		RealArgs = [UniqueArg],
		A = UniqueArg
		;
		length(RealArgs,Length),
		Length > 1,
		A = RealArgs
		;
		RealArgs = [],
		A = _
	),
	call(PredName,A,Output).

/*
BRACHYLOG_PLUS
*/
brachylog_plus('integer':I,'integer':AbsoluteValue) :-
	AbsoluteValue #= abs(I),
	label([I,AbsoluteValue]).
brachylog_plus('float':F,'float':AbsoluteValue) :-
	AbsoluteValue is abs(F).
brachylog_plus(L,Sum) :-
	is_list(L),
	\+ (maplist(is_list,L)),
	brachylog_plus_(L,Sum).
brachylog_plus(ListOfLists,Sums) :-
	maplist(is_list,ListOfLists),
	ListOfLists = [H|_],
	(
		length(H,Length),
		maplist(length_(Length),ListOfLists),
		transpose(ListOfLists,Transpose),
		maplist(brachylog_plus_,Transpose,Sums)
		;
		\+ (maplist(length_(Length),ListOfLists)),
		throw('Lists must have have the same length to be added')
	).
	
brachylog_plus_([],'integer':0).
brachylog_plus_([TypeI:I|T],TypeS:Sum) :-
	brachylog_plus_(T,TypeF:F),
	(
		TypeI = 'integer',
		TypeF = 'integer',
		Sum #= I + F,
		TypeS = 'integer'
		;
		TypeS = 'float',
		(
			TypeF = 'float',
			TypeI = 'integer',
			label([I])
			;
			TypeI = 'float',
			nonvar(I),
			TypeF = 'integer',
			label([F])
			;
			TypeF = 'float',
			TypeI = 'float',
			nonvar(I)
		),
		Sum is I + F
	).
	
/*
BRACHYLOG_MINUS
*/
brachylog_minus('integer':I,'integer':J) :-
	J #= -I,
	label([I,J]).
brachylog_minus('float':I,'float':J) :-
	J is -I.
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
