:- module(tokenize, [tokenize/2]).

:- use_module(symbols).

/*
TOKENIZE
*/
tokenize([],[]).

tokenize([Variable|T], ['variable':Variable|T2]) :-
	is_variable(Variable),
	tokenize(T,T2).
	
tokenize(['?'|T], ['variable':'Input'|T2]) :-
	tokenize(T,T2).

tokenize(['.'|T], ['variable':'Output'|T2]) :-
	tokenize(T,T2).
	
tokenize(['<','='|T], ['predicate':PredName|T2]) :-
	token_predicate('<=',PredName),
	tokenize(T,T2).

tokenize(['>','='|T], ['predicate':PredName|T2]) :-
	token_predicate('>=',PredName),
	tokenize(T,T2).
	
tokenize([Predicate|T], ['predicate':PredName|T2]) :-
	(
		is_predicate(Predicate)
		;
		Predicate = '&'
		;
		Predicate = '='
		;
		Predicate = '<'
		;
		Predicate = '>'
		;
		Predicate = '+'
		;
		Predicate = '-'
		;
		Predicate = '*'
		;
		Predicate = '/'
		;
		Predicate = '%'
		;
		Predicate = '^'
	),
	token_predicate(Predicate,PredName),
	tokenize(T,T2).

tokenize(['"'|T], ['variable':Variable|T2]) :-
	tokenize_string(['"'|T],Rest,Variable),
	tokenize(Rest,T2).
	
tokenize(['_',Digit|T], ['variable':Type:N|T2]) :-
	is_digit(Digit),
	tokenize_number([Digit|T],Rest,Type:X),
	N is -X,
	tokenize(Rest,T2).
	
tokenize(['_','_'|T], T2) :-
	tokenize(T,T2).
	
tokenize([Digit|T], ['variable':Type:X|T2]) :-
	is_digit(Digit),
	tokenize_number([Digit|T],Rest,Type:X),
	tokenize(Rest,T2).
	
tokenize(['['|T], ['variable':List|T2]) :-
	tokenize_list(['['|T],Rest,List),
	tokenize(Rest,T2).
	
tokenize([Modifier,Variable|T], ['variable':RealVariable|T2]) :-
	is_modifier(Modifier),
	is_variable(Variable),
	token_variable(Modifier:Variable,RealVariable),
	tokenize(T,T2).

tokenize([Modifier,Predicate|T], ['predicate':PredName|T2]) :-
	is_modifier(Modifier),
	\+ (is_variable(Predicate)),
	token_predicate(Modifier:Predicate,PredName),
	tokenize(T,T2).
	
tokenize([ControlFlow|T],['control':ControlFlow|T2]) :-
	is_control_flow(ControlFlow),
	tokenize(T,T2).
	

/*
TOKENIZE_STRING
*/
tokenize_string(['"'|T],Rest,'string':T2) :-
	tokenize_string_(T,Rest,T2).
	
tokenize_string_([],[],[]).
tokenize_string_([X,'"'|Rest],Rest,[X]) :-
	X \= '\\',!.
tokenize_string_(['\\','"'|T],Rest,['\\','"'|T2]) :-
	tokenize_string_(T,Rest,T2).
tokenize_string_([X|T],Rest,[X|T2]) :-
	X \= '"',
	tokenize_string_(T,Rest,T2).


/*
TOKENIZE_NUMBER
*/	
tokenize_number(N,Rest,Type:Number) :-
	tokenize_number_(N,Rest,T2),
	(
		member('.',T2),!,
		Type = 'float'
		;
		Type = 'integer'
	),
	atomic_list_concat(T2,A),
	atom_number(A,Number).
	
tokenize_number_([],[],[]).
tokenize_number_(['.',I|T],Rest,['.',J|T2]) :-
	is_digit(I),
	atom_number(I,J),
	tokenize_integer(T,Rest,T2).
tokenize_number_(['.'],['.'],[]).
tokenize_number_(['.',X|T],['.',X|T],[]) :-
	\+ (is_digit(X)).
tokenize_number_([X|T],[X|T],[]) :-
	\+ (is_digit(X)),
	X \= '.'.
tokenize_number_([I|T],Rest,[J|T2]) :-
	is_digit(I),
	atom_number(I,J),
	tokenize_number_(T,Rest,T2).

tokenize_integer([],[],[]).
tokenize_integer([I|T],Rest,[J|T2]) :-
	is_digit(I),
	atom_number(I,J),
	tokenize_integer(T,Rest,T2).
tokenize_integer([X|T],[X|T],[]) :-
	\+ (is_digit(X)).
	
	
/*
TOKENIZE_LIST
*/
tokenize_list(['['|T],Rest,List) :-
	isolate_list(T,L,Rest),
	tokenize(L,List).
	
isolate_list(T,List,Rest) :-
	isolate_list(T,1,[],L,Rest),
	reverse(L,List).
isolate_list([],_,L,L,[]).
isolate_list([']'|T],1,L,L,T).
isolate_list([']'|T],X,L,M,Rest) :-
	X > 1,
	Y is X - 1,
	isolate_list(T,Y,[']'|L],M,Rest).
isolate_list(['['|T],X,L,M,Rest) :-
	Y is X + 1,
	isolate_list(T,Y,['['|L],M,Rest).
isolate_list([H|T],X,L,M,Rest) :-
	H \= '[',
	H \= ']',
	isolate_list(T,X,[H|L],M,Rest).
