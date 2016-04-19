:- use_module(tokenize).
:- use_module(symbols).

/*
PARSE
*/
parse(Code,Program) :-
	atom_chars(Code,SplittedCode),
	tokenize(SplittedCode,Tokens),
	fill_implicit_variables(Tokens,FilledTokens),
	fix_lists(FilledTokens,Program),
	transpile(Program,Predicates),
	open('compiled_brachylog.pl', write, File),
	maplist(write_to_file(File),Predicates),
	close(File).
	

/*
FILL_IMPLICIT_VARIABLES
*/
fill_implicit_variables(Tokens,Program) :-
	fill_implicit_variables(Tokens,0,Program).

fill_implicit_variables([],_,[]).
	
fill_implicit_variables(['predicate':A,Type:B|T],I,['predicate':A,'variable':V|T2]) :-
	Type \= 'variable',
	atom_concat('V',I,V),
	J is I + 1,
	fill_implicit_variables([Type:B|T],J,T2).
	
fill_implicit_variables(['predicate':A],I,['predicate':A,'variable':V]) :-
	atom_concat('V',I,V).
	
fill_implicit_variables(['predicate':A,'variable':B|T],I,['predicate':A,'variable':B|T2]) :-
	fill_implicit_variables(T,I,T2).

fill_implicit_variables([Type:A|T],I,[Type:A|T2]) :-
	Type \= 'predicate',
	fill_implicit_variables(T,I,T2).
	

/*
FIX_LISTS
*/
fix_lists([],[]).

fix_lists(['variable':List|T],['variable':FixedList|T2]) :-
	is_list(List),
	fix_list(List,FixedList),
	fix_lists(T,T2).
	
fix_lists([X|T],[X|T2]) :-
	(
		X = 'variable':L,
		\+ (is_list(L))
		;
		X \= 'variable':_
	),
	fix_lists(T,T2).
	
	
fix_list([],[]).
fix_list(['control':':'|T],T2) :-
	fix_list(T,T2).
fix_list([X|T],[Y|T2]) :-
	X \= 'control':':',
	(
		X = 'variable':L,
		is_list(L),
		fix_list(L,Y)
		;
		X = 'variable':Y
		;
		X = 'predicate':_,
		Y = X
		;
		Y = X
	),
	fix_list(T,T2).
	
	
	
/*
TRANSPILE
*/
transpile(Program,[['brachylog_main(Input,Output) :-\n    1=1'|T]|OtherPredicates]) :-
	transpile_(Program,['Input'],no,no,0,[T|OtherPredicates]).
	
transpile_([],_,_,_,_,[['.\n']]).

transpile_(['variable':B|T],A,Reverse,Negate,PredNumber,[[Unification|T2]|OtherPredicates]) :-
	A \= [],
	(
		A = ['variable':L],
		is_list(L),
		brachylog_list_to_atom(A,Var1)
		;
		length(A,L),
		L > 1,
		brachylog_list_to_atom(A,Var1)
		;
		A = [Type:L],
		term_to_atom(Type:L,Var1)
		;
		A = [L],
		Var1 = L
	),
	(
		is_list(B),
		brachylog_list_to_atom(B,Var2)
		;
		B = _:_,
		term_to_atom(B,Var2)
		;
		Var2 = B
	),
	(
		Negate = yes,
		UnificationAtom = ' \\= '
		;
		Negate = no,
		UnificationAtom = ' = '
	),
	(
		Reverse = no,
		atomic_list_concat([',\n    ',Var2,UnificationAtom,Var1],Unification),
		transpile_(T,[B],no,no,PredNumber,[T2|OtherPredicates])
		;
		Reverse = yes,
		atomic_list_concat([',\n    ',Var1,UnificationAtom,Var2],Unification),
		transpile_(T,[B],no,no,PredNumber,[T2|OtherPredicates])
	).
	
transpile_(['variable':B|T],[],_,_,PredNumber,[T2|OtherPredicates]) :-
	transpile_(T,[B],no,no,PredNumber,[T2|OtherPredicates]).
	
transpile_(['predicate':P,'variable':B|T],A,Reverse,Negate,PredNumber,[[Predicate|T2]|OtherPredicates]) :-
	A \= [],
	(
		A = ['variable':L],
		is_list(L),
		brachylog_list_to_atom(A,Var1)
		;
		length(A,L),
		L > 1,
		brachylog_list_to_atom(A,Var1)
		;
		A = [Type:L],
		term_to_atom(Type:L,Var1)
		;
		A = [L],
		Var1 = L
	),
	(
		is_list(B),
		brachylog_list_to_atom(B,Var2)
		;
		B = _:_,
		term_to_atom(B,Var2)
		;
		Var2 = B
	),
	(
		Negate = yes,
		NegateAtom = '\\+ '
		;
		Negate = no,
		NegateAtom = ''
	),
	(
		Reverse = no,
		atomic_list_concat([',\n    ',NegateAtom,P,'(',Var1,',',Var2,')'],Predicate),
		transpile_(T,[B],no,no,PredNumber,[T2|OtherPredicates])
		;
		Reverse = yes,
		atomic_list_concat([',\n    ',NegateAtom,P,'(',Var2,',',Var1,')'],Predicate),
		transpile_(T,[B],no,no,PredNumber,[T2|OtherPredicates])
	).

transpile_(['control':','|T],_,_,_,PredNumber,[T2|OtherPredicates]) :-
	transpile_(T,[],no,no,PredNumber,[T2|OtherPredicates]).
	
transpile_(['control':';'|T],_,_,_,PredNumber,[['\n    ;\n    1=1'|T2]|OtherPredicates]) :-
	transpile_(T,[],no,no,PredNumber,[T2|OtherPredicates]).
	
transpile_(['control':'('|T],B,_,Negate,PredNumber,[[Parenthesis|T2]|OtherPredicates]) :-
	(
		Negate = yes,
		Parenthesis = ',\n    \\+ (\n    1=1'
		;
		Negate = no,
		Parenthesis = ',\n    (\n    1=1'
	),
	transpile_(T,B,no,no,PredNumber,[T2|OtherPredicates]).
	
transpile_(['control':')'|T],B,_,_,PredNumber,[['\n    )'|T2]|OtherPredicates]) :-
	transpile_(T,B,no,no,PredNumber,[T2|OtherPredicates]).
	
transpile_(['control':'!'|T],B,_,_,PredNumber,[[',\n    !'|T2]|OtherPredicates]) :-
	transpile_(T,B,no,no,PredNumber,[T2|OtherPredicates]).
	
transpile_(['control':'\\'|T],B,_,_,PredNumber,[[',\n    fail'|T2]|OtherPredicates]) :-
	transpile_(T,B,no,no,PredNumber,[T2|OtherPredicates]).
	
transpile_(['control':'~'|T],B,Reverse,Negate,PredNumber,[T2|OtherPredicates]) :-
	(
		Reverse = yes,
		NewReverse = no
		;
		Reverse = no,
		NewReverse = yes
	),
	transpile_(T,B,NewReverse,Negate,PredNumber,[T2|OtherPredicates]).
	
transpile_(['control':'\''|T],B,Reverse,Negate,PredNumber,[T2|OtherPredicates]) :-
	(
		Negate = yes,
		NewNegate = no
		;
		Negate = no,
		NewNegate = yes
	),
	transpile_(T,B,Reverse,NewNegate,PredNumber,[T2|OtherPredicates]).
	
transpile_(['control':':',Type:A|T],B,_,_,PredNumber,[T2|OtherPredicates]) :-
	(
		Type = 'variable'
		;
		Type = 'predicate'
	),
	append(B,[A],NewVar),
	transpile_(T,NewVar,no,no,PredNumber,[T2|OtherPredicates]).
	
transpile_(['control':'\n'|T],_,_,_,PredNumber,[['.\n'],[PredHead|T2]|OtherPredicates]) :-
	J is PredNumber + 1,
	atomic_list_concat(['brachylog_predicate_',J,'(Input,Output) :-\n    1=1'],PredHead),
	transpile_(T,['Input'],no,no,J,[T2|OtherPredicates]).
	
	
brachylog_list_to_atom(List,Atom) :-
	brachylog_list_to_atom_(List,T2),
	atomic_list_concat(['[',T2,']'],Atom).
	
brachylog_list_to_atom_([],'').
brachylog_list_to_atom_([A],AtomA) :-
	(
		is_list(A),
		brachylog_list_to_atom(A,AtomA)
		;
		A = _:_,
		term_to_atom(A,AtomA)
		;
		AtomA = A
	).
brachylog_list_to_atom_([A,B|T],Atom) :-
	(
		is_list(A),
		brachylog_list_to_atom(A,AtomA)
		;
		A = _:_,
		term_to_atom(A,AtomA)
		;
		AtomA = A
	),
	brachylog_list_to_atom_([B|T],T2),
	atomic_list_concat([AtomA,',',T2],Atom).
	
	
/*
WRITE_TO_FILE
*/
write_to_file(File,[]) :-
	write(File,'\n\n').
write_to_file(File, [H|T]) :-
	write(File,H),
	write_to_file(File,T).