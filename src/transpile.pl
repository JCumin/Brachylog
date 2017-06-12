/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
____            ____
\   \          /   /
 \   \  ____  /   /
  \   \/    \/   /
   \     /\     /     BRACHYLOG       
    \   /  \   /      A terse declarative logic programming language
    /   \  /   \    
   /     \/     \     Written by Julien Cumin - 2017
  /   /\____/\   \    https://github.com/JCumin/Brachylog
 /   /  ___   \   \
/___/  /__/    \___\
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- module(transpile, [parse/2,
                      parse_no_file/2,
                      parse_argument/2,
                      contains_write/1
                     ]).

:- use_module(tokenize).
:- use_module(symbols).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   PARSE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
parse(Code, TranspiledPath) :-
    parse_no_file(Code, Predicates),
    open(TranspiledPath, write, File),
    maplist(write_to_file(File), Predicates),
    close(File).
    
parse_no_file(Code, Predicates) :-
    atom_chars(Code, SplittedCode),
    tokenize(SplittedCode, TokensNoOutputs),
    append_trailing_output(TokensNoOutputs, Tokens),
    fix_predicates(Tokens, FixedPredicates),
    fix_metapredicates(FixedPredicates, FixedMetapredicates),
    fill_implicit_variables(FixedMetapredicates, FilledTokens),
    fix_lists(FilledTokens, Program),
    transpile(Program, Predicates),
    !.
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   PARSE_ARGUMENT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
parse_argument(Arg, Term) :-
    (   atom(Arg),
        AtomArg = Arg
    ;   \+ atom(Arg),
        term_to_atom(Arg, AtomArg)
    ),
    atom_chars(AtomArg, SplittedArg),
    tokenize(SplittedArg, Token),
    fix_lists(Token, Program),
    transpile(Program, Parsed),
    !,
    reverse(Parsed, [TempMainPredicate|_]),
    nth0(3, TempMainPredicate, Atom),
    atom_concat(',\n    ', AtomT, Atom),
    atom_concat(ParsedArg, ' = Input', AtomT),
    term_to_atom(Term, ParsedArg)
    ;
    throw('Incorrect variable format.').


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   APPEND_TRAILING_OUTPUT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
append_trailing_output([], ['variable':'Output']).
append_trailing_output(['control':'\n'|T], ['variable':'Output','control':'\n'|T2]) :-
    append_trailing_output(T, T2).
append_trailing_output(['control':'}'|T], ['variable':'Output','control':'}'|T2]) :-
    append_trailing_output(T, T2).
append_trailing_output(['control':'|'|T], ['variable':'Output','control':'|'|T2]) :-
    append_trailing_output(T, T2).
append_trailing_output([H|T], [H|T2]) :-
    H \= 'control':'\n',
    H \= 'control':'}',
    H \= 'control':'|',
    append_trailing_output(T, T2).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   FIX_PREDICATES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
fix_predicates(Tokens, FixedPredicates) :-
    fix_predicates(Tokens, 1, L),
    append(L, FixedPredicates).
    
fix_predicates([], _, [[]]).
fix_predicates(['control':'{'|T], I, [['predicate':PredName:0|Rest], ['control':'\n'|Predicate]|AllOtherPredicates]) :-
    atomic_list_concat(['brachylog_predicate_',I], PredName),
    J is I + 1,
    fix_predicates_(T, J, [Predicate|OtherPredicates1], Z, Remaining),
    fix_predicates(Remaining, Z, [Rest|OtherPredicates2]),
    append(OtherPredicates1, OtherPredicates2, AllOtherPredicates).
fix_predicates(['control':'\n'|T], I, [[],['control':'\n'|Rest]|OtherPredicates]) :-
    J is I + 1,
    fix_predicates(T, J, [Rest|OtherPredicates]).
fix_predicates([Type:A|T], I, [[Type:A|Rest]|OtherPredicates]) :-
    \+ (Type = 'control', A = '{'),
    \+ (Type = 'control', A = '}'),
    \+ (Type = 'control', A = '\n'),
    fix_predicates(T, I, [Rest|OtherPredicates]).
    
fix_predicates_([], _, [[]]).
fix_predicates_(['control':'{'|T], I, [['predicate':PredName:0|Rest], ['control':'\n'|Predicate]|AllOtherPredicates], Z, Remaining) :-
    atomic_list_concat(['brachylog_predicate_',I], PredName),
    J is I + 1,
    fix_predicates_(T, J, [Predicate|OtherPredicates1], Z2, Remaining2),
    fix_predicates_(Remaining2, Z2, [Rest|OtherPredicates2], Z, Remaining),
    append(OtherPredicates1, OtherPredicates2, AllOtherPredicates).
fix_predicates_(['control':'}'|T], I, [[]], I, T).
fix_predicates_([Type:A|T], I, [[Type:A|Rest]|OtherPredicates], Z, Remaining) :-
    \+ (Type = 'control', A = '{'),
    \+ (Type = 'control', A = '}'),
    \+ (Type = 'control', A = '\n'),
    fix_predicates_(T, I, [Rest|OtherPredicates], Z, Remaining).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   FIX_METAPREDICATES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
fix_metapredicates([], []).
fix_metapredicates(['predicate':PredName:Sub,'metapredicate':MetapredName:Sup|T], ['predicate':PredName:Sub:MetapredName:Sup|T2]) :-
    fix_metapredicates(T, T2).
fix_metapredicates(['predicate':PredName:Sub|T], ['predicate':PredName:Sub:'no':0|T2]) :-
    fix_metapredicates(T, T2).
fix_metapredicates([H|T], [H|T2]) :-
    fix_metapredicates(T, T2).

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   FILL_IMPLICIT_VARIABLES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
fill_implicit_variables(Tokens, Program) :-
    fill_implicit_variables(Tokens, 0, Program).

fill_implicit_variables([], _, []).
fill_implicit_variables(['control':':','predicate':A|T], I, ['control':':','predicate':A|T2]) :-
    fill_implicit_variables(T, I, T2).
fill_implicit_variables(['predicate':A,Type:B|T], I, ['predicate':A,'variable':V|T2]) :-
    Type \= 'variable',
    atom_concat('V', I, V),
    J is I + 1,
    fill_implicit_variables([Type:B|T], J, T2).
fill_implicit_variables(['predicate':A], I, ['predicate':A,'variable':V]) :-
    atom_concat('V', I, V).
fill_implicit_variables(['predicate':A,'variable':B|T], I, ['predicate':A,'variable':B|T2]) :-
    fill_implicit_variables(T, I, T2).
fill_implicit_variables(['control':H,Type:B|T], I, ['control':H,'variable':V|T2]) :-
    Type \= 'variable',
    (   H = '∧'
    ;   H = '∨'
    ),
    atom_concat('V', I, V),
    J is I + 1,
    fill_implicit_variables([Type:B|T], J, T2).
fill_implicit_variables([Type:A|T], I, [Type:A|T2]) :-
    Type \= 'predicate',
    \+ (Type = 'control', A = ':', T = ['predicate':_|_]),
    \+ (Type = 'control', A = '∧', T \= ['variable':_|_]),
    \+ (Type = 'control', A = '∨', T \= ['variable':_|_]),
    fill_implicit_variables(T, I, T2).
   
   
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   FIX_LISTS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
fix_lists([], []).
fix_lists(['variable':List|T], ['variable':FixedList|T2]) :-
    is_list(List),
    fix_list(List, FixedList),
    fix_lists(T, T2).
fix_lists([X|T], [X|T2]) :-
    (   X = 'variable':L,
        \+ (is_list(L))
    ;   X \= 'variable':_
    ),
    fix_lists(T, T2).

fix_list([], []).
fix_list(['control':','|T], T2) :-
    fix_list(T, T2).
fix_list([X|T], [Y|T2]) :-
    X \= 'control':',',
    (   X = 'variable':L,
        is_list(L),
        fix_list(L, Y)
    ;   X = 'variable':Y
    ;   X = 'predicate':_,
        Y = X
    ;   Y = X
    ),
    fix_list(T, T2).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TRANSPILE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
transpile(Program, [[':- style_check(-singleton).'],
                    [':- use_module(library(clpfd)).'],
                    [':- use_module(predicates).'],
                    [':- use_module(metapredicates).'],
                    [':- use_module(constraint_variables).\n'],
                    ['brachylog_main(_, Input,Output) :-\n    Name = brachylog_main,\n',
                    ConstraintVariables,
                    '    (1=1'|MainPred]|OtherPredicates]) :-
    constraint_variables(ConstraintVariables),
    transpile_(Program, 'Input', no, no, 0, 0, [T|OtherPredicates]),
    reverse(T, [_|RT]),
    reverse(RT,T2),
    append(T2, ['\n    ),\n    ((Output = integer:_ ; Output = [_|_], forall(member(E, Output), E = integer:_)) -> brachylog_label(default, Output, _) ; true).\n'], MainPred).
    
transpile_([], _, _, _, _, _, [['\n    ).\n']]).
transpile_(['variable':B|T], A, Reverse, Negate, AppendNumber, PredNumber, [[Unification|T2]|OtherPredicates]) :-
    A \= 'nothing',
    (   is_list(A),
        brachylog_list_to_atom(A, Var1)
    ;   A = Type:L,
        term_to_atom(Type:L, Var1)
    ;   A = Var1
    ),
    (   is_list(B),
        brachylog_list_to_atom(B, Var2)
    ;   B = _:_,
        term_to_atom(B, Var2)
    ;   Var2 = B
    ),
    (   Negate = yes,
        UnificationAtom = ' \\= '
    ;   Negate = no,
        UnificationAtom = ' = '
    ),
    (   Reverse = no,
        atomic_list_concat([',\n    ',Var2,UnificationAtom,Var1], Unification),
        transpile_(T, B, no, no, AppendNumber, PredNumber, [T2|OtherPredicates])
    ;   Reverse = yes,
        atomic_list_concat([',\n    ',Var1,UnificationAtom,Var2], Unification),
        transpile_(T, B, no, no, AppendNumber, PredNumber, [T2|OtherPredicates])
    ).
transpile_(['variable':B|T], 'nothing', _, _, AppendNumber, PredNumber, [T2|OtherPredicates]) :-
    transpile_(T, B, no, no, AppendNumber, PredNumber, [T2|OtherPredicates]). 
transpile_(['predicate':P:Sub:Meta:Sup,'variable':B|T], A, Reverse, Negate, AppendNumber, PredNumber, [[Predicate|T2]|OtherPredicates]) :-
    A \= 'nothing',
    (   P = 'brachylog_call_predicate',
        (   is_list(A),
            reverse(A, RA)
        ;   RA = [A]
        ),
        A3 = ['Name'|RA],
        reverse(A3, A2)
    ;   P \= 'brachylog_call_predicate',
        A2 = A
    ),
    (   is_list(A2),
        brachylog_list_to_atom(A2, Var1)
    ;   A2 = Type:L,
        term_to_atom(Type:L, Var1)
    ;   A2 = Var1
    ),
    (   is_list(B),
        brachylog_list_to_atom(B, Var2)
    ;   B = _:_,
        term_to_atom(B, Var2)
    ;   Var2 = B
    ),
    (   Negate = yes,
        NegateAtom = '\\+ '
    ;   Negate = no,
        NegateAtom = ''
    ),
    (   Reverse = no ->
        PredName = P
    ;   atomic_list_concat([P,'_reversed'], PredName)
    ),
    (   Meta = no ->
        atomic_list_concat([',\n    ',NegateAtom,PredName,'(',Sub,',',Var1,',',Var2,')'], Predicate)
    ;   atomic_list_concat([',\n    ',NegateAtom,Meta,'(',Sup,',',PredName,',',Sub,',',Var1,',',Var2,')'], Predicate)
    ),
    transpile_(T, B, no, no, AppendNumber, PredNumber, [T2|OtherPredicates]).
transpile_(['control':'∧'|T], _, _, _, AppendNumber, PredNumber, [T2|OtherPredicates]) :-
    transpile_(T, 'nothing', no, no, AppendNumber, PredNumber, [T2|OtherPredicates]).
transpile_(['control':'&'|T], _, _, _, AppendNumber, PredNumber, [T2|OtherPredicates]) :-
    transpile_(T, 'Input', no, no, AppendNumber, PredNumber, [T2|OtherPredicates]).
transpile_(['control':'`'|T], B, _, _, AppendNumber, PredNumber, [['\n    *->\n    1=1'|T2]|OtherPredicates]) :-
    transpile_(T, B, no, no, AppendNumber, PredNumber, [T2|OtherPredicates]).    
transpile_(['control':'∨'|T], _, _, _, AppendNumber, PredNumber, [['\n    ;\n    1=1'|T2]|OtherPredicates]) :-
    transpile_(T, 'nothing', no, no, AppendNumber, PredNumber, [T2|OtherPredicates]).
transpile_(['control':'('|T], B, _, Negate, AppendNumber, PredNumber, [[Parenthesis|T2]|OtherPredicates]) :-
    (   Negate = yes,
        Parenthesis = ',\n    \\+ (\n    1=1'
    ;   Negate = no,
        Parenthesis = ',\n    (\n    1=1'
    ),
    transpile_(T, B, no, no, AppendNumber, PredNumber, [T2|OtherPredicates]).
transpile_(['control':')'|T], B, _, _, AppendNumber, PredNumber, [['\n    )'|T2]|OtherPredicates]) :-
    transpile_(T, B, no, no, AppendNumber, PredNumber, [T2|OtherPredicates]). 
transpile_(['control':'!'|T], B, _, _, AppendNumber, PredNumber, [[',\n    !'|T2]|OtherPredicates]) :-
    transpile_(T, B, no, no, AppendNumber, PredNumber, [T2|OtherPredicates]). 
transpile_(['control':'⊥'|T], B, _, _, AppendNumber, PredNumber, [[',\n    false'|T2]|OtherPredicates]) :-
    transpile_(T, B, no, no, AppendNumber, PredNumber, [T2|OtherPredicates]).  
transpile_(['control':'~'|T], B, Reverse, Negate, AppendNumber, PredNumber, [T2|OtherPredicates]) :-
    (   Reverse = yes,
        NewReverse = no
    ;   Reverse = no,
        NewReverse = yes
    ),
    transpile_(T, B, NewReverse, Negate, AppendNumber, PredNumber, [T2|OtherPredicates]).   
transpile_(['control':'¬'|T], B, Reverse, Negate, AppendNumber, PredNumber, [T2|OtherPredicates]) :-
    (   Negate = yes,
        NewNegate = no
    ;   Negate = no,
        NewNegate = yes
    ),
    transpile_(T, B, Reverse, NewNegate, AppendNumber, PredNumber, [T2|OtherPredicates]).
transpile_(['control':';',Type:A|T], B, _, _, AppendNumber, PredNumber, [T2|OtherPredicates]) :-
    (   Type = 'variable'
    ;   Type = 'predicate'
    ),
    append([B], [A], NewVar),
    transpile_(T, NewVar, no, no, AppendNumber, PredNumber, [T2|OtherPredicates]).
transpile_(['control':',','variable':A|T], B, _, _, AppendNumber, PredNumber, [[Append|T2]|OtherPredicates]) :-
    (   is_list(A),
        brachylog_list_to_atom(A, Arg1)
    ;   A = TypeA:LA,
        term_to_atom(TypeA:LA, Arg1)
    ;   A = Arg1
    ),
    (   is_list(B),
        brachylog_list_to_atom(B, Arg2)
    ;   B = TypeB:LB,
        term_to_atom(TypeB:LB, Arg2)
    ;   B = Arg2
    ),
    atomic_list_concat(['AppendTemp',AppendNumber],TempVar),
    atomic_list_concat([',\n    ',
                        '(brachylog_concatenate(default,',
                        '[',Arg2,',',Arg1,']',
                        ',',TempVar,') -> true ; is_brachylog_list(',
                        Arg2,
                        '), brachylog_concatenate(default,',
                        '[',Arg2,',[',Arg1,']]',
                        ',',TempVar,') -> true ; brachylog_concatenate(default,',
                        '[[',Arg2,'],[',Arg1,']],',TempVar,'))'
                       ], Append),
    NewAppendNumber is AppendNumber + 1,
    transpile_(T, TempVar, no, no, NewAppendNumber, PredNumber, [T2|OtherPredicates]).
transpile_(['control':'\n'|T], _, _, _, AppendNumber, PredNumber, [['\n    ).\n'],[PredHead|T2]|OtherPredicates]) :-
    J is PredNumber + 1,
    constraint_variables(ConstraintVariables),
    atomic_list_concat(['brachylog_predicate_',
                        J,
                        '(_, Input,Output) :-\n    Name = brachylog_predicate_',
                        J,
                        ',\n',
                        ConstraintVariables,
                        '    (1=1'], PredHead),
    transpile_(T, 'Input', no, no, AppendNumber, J, [T2|OtherPredicates]).
transpile_(['control':'|'|T], _, _, _, AppendNumber, PredNumber, [['\n    ).\n'],[PredHead|T2]|OtherPredicates]) :-
    (   PredNumber = 0,
        PredName = 'brachylog_main'
    ;   PredNumber \= 0,
        atomic_list_concat(['brachylog_predicate_',PredNumber], PredName)
    ),
    constraint_variables(ConstraintVariables),
    atomic_list_concat([PredName,
                        '(_, Input,Output) :-\n    Name = ',
                        PredName,
                        ',\n',
                        ConstraintVariables,
                        '    (1=1'], PredHead),
    transpile_(T, 'Input', no, no, AppendNumber, PredNumber, [T2|OtherPredicates]).
    

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   CONTAINS_WRITE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
contains_write(Code) :-
    atom_chars(Code, SplittedCode),
    tokenize(SplittedCode, Tokens),
    fix_predicates(Tokens, FixedPredicates),
    (   member(predicate:brachylog_write:_, FixedPredicates)
    ;   member(predicate:brachylog_writeln:_, FixedPredicates)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   CONSTRAINT_VARIABLES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
constraint_variables(ConstraintVariables) :-
    findall(S, (member(X, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']),
                atomic_list_concat(['    constraint',X,'(Constraint',X,'),\n'], S)),
            Ss
            ),
    atomic_list_concat(Ss, ConstraintVariables).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_LIST_TO_ATOM
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_list_to_atom(List, Atom) :-
    brachylog_list_to_atom_(List, T2),
    atomic_list_concat(['[',T2,']'], Atom).
    
brachylog_list_to_atom_([], '').
brachylog_list_to_atom_([A], AtomA) :-
    (   is_list(A),
        brachylog_list_to_atom(A, AtomA)
    ;   A = _:_,
        term_to_atom(A, AtomA)
    ;   \+ is_list(A),
        A \= _:_,
        AtomA = A
    ).
brachylog_list_to_atom_([A,B|T], Atom) :-
    (   is_list(A),
        brachylog_list_to_atom(A, AtomA)
    ;   A = _:_,
        term_to_atom(A, AtomA)
    ;   \+ is_list(A),
        A \= _:_,
        AtomA = A
    ),
    brachylog_list_to_atom_([B|T], T2),
    atomic_list_concat([AtomA,',',T2], Atom).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   WRITE_TO_FILE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
write_to_file(File, []) :-
    write(File, '\n\n').
write_to_file(File, [H|T]) :-
    write(File, H),
    write_to_file(File, T).
