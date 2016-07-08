/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
____            ____
\   \          /   /
 \   \  ____  /   /
  \   \/    \/   /
   \     /\     /     BRACHYLOG       
    \   /  \   /      A terse declarative logic programming language
    /   \  /   \    
   /     \/     \     Written by Julien Cumin - 2016
  /   /\____/\   \    https://github.com/JCumin/Brachylog
 /   /  ___   \   \
/___/  /__/    \___\
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- module(predicates, [brachylog_apply/2,
                       brachylog_behead/2,
                       brachylog_concatenate/2,
                       brachylog_duplicates/2,
                       brachylog_enumerate/2,
                       brachylog_findall/2,
                       brachylog_group/2,
                       brachylog_head/2,
                       brachylog_iterate/2,
                       brachylog_length/2,
                       brachylog_member/2,
                       brachylog_order/2,
                       brachylog_permute/2,
                       brachylog_reverse/2,
                       brachylog_subset/2,
                       brachylog_tail/2,
                       brachylog_void/2,
                       brachylog_write/2,
                       brachylog_xterminate/2,
                       brachylog_yield/2,
                       brachylog_zip/2,
                       brachylog_call_predicate/2,
                       brachylog_plus/2,
                       brachylog_minus/2,
                       brachylog_multiply/2,
                       brachylog_divide/2,
                       brachylog_power/2,
                       brachylog_greater/2,
                       brachylog_less/2,
                       brachylog_lessequal/2,
                       brachylog_greaterequal/2,
                       brachylog_equals/2,
                       brachylog_modulo/2
                      ]).
                       
:- use_module(library(clpfd)).
:- use_module(math_predicates).
:- use_module(utils).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_APPLY
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_apply(L,Y) :-
	is_brachylog_list(L),
	reverse(L,[PredName|Args]),
	(
		Args = [Arg],
		is_brachylog_list(Arg)
		-> true
		;
		Arg = Args
	),
	brachylog_apply_append_predname(Arg,PredName,NewArg),
	maplist(brachylog_call_predicate,NewArg,Y).
	
brachylog_apply_append_predname([],_,[]).
brachylog_apply_append_predname([H|T],PredName,[[H,PredName,'ignore_calling_predicate']|T2]) :-
	brachylog_apply_append_predname(T,PredName,T2).
    

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_BEHEAD
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */        
brachylog_behead('string':[_|T],'string':T).
brachylog_behead('integer':0,'integer':0).
brachylog_behead('integer':I,'integer':J) :-
    H #\= 0,
    integer_value('integer':Sign:[H|T],I),
    integer_value('integer':Sign:T,J).
brachylog_behead('float':F,'float':G) :-
    number_codes(F,L),
    brachylog_behead_float(L,M),
    number_codes(G,M).
brachylog_behead([_|T],T).

brachylog_behead_float([],[]).
brachylog_behead_float([48|T],[48|T2]) :-
    brachylog_behead_float(T,T2).
brachylog_behead_float([46|T],[46|T2]) :-
    brachylog_behead_float(T,T2).
brachylog_behead_float([H|T],[48|T2]) :-
    H \= 46,
    H \= 48,
    brachylog_behead_float_(T,T2).

brachylog_behead_float_([],[]).
brachylog_behead_float_([H|T],[H|T2]) :-
    brachylog_behead_float_(T,T2).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_CONCATENATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_concatenate('string':L,'string':L).
brachylog_concatenate('integer':I,'integer':I).
brachylog_concatenate('float':F,'float':F).
brachylog_concatenate([H|T],L) :-
    brachylog_concatenate(T,H,L).

brachylog_concatenate([],L,L).
brachylog_concatenate(['string':H|T],'string':S,L) :-
    append(S,H,I),
    brachylog_concatenate(T,'string':I,L).
brachylog_concatenate([L|T],L2,L3) :-
    is_brachylog_list(L),
    is_brachylog_list(L2),
    append(L2,L,M),
    brachylog_concatenate(T,M,L3).
brachylog_concatenate(L,'integer':I,Z) :-
    AI #= abs(I),
    (
        I #= 0,
        HI #= 0,
        TI = []
        ;
        HI #\= 0
    ),
    integer_value('integer':'positive':[HI|TI],AI),
    brachylog_concatenate_(L,[HI|TI],Z).
    
brachylog_concatenate_([],[HZ|TZ],'integer':Z) :-
    (
        Z #= 0,
        HZ #= 0,
        TZ = []
        ;
        HZ #\= 0
    ),
    integer_value('integer':'positive':[HZ|TZ],Z).
brachylog_concatenate_(['integer':I|T],L,'integer':Z) :-
    AI #= abs(I),
    (
        I #= 0,
        HI #= 0,
        TI = []
        ;
        HI #\= 0
    ),
    integer_value('integer':'positive':[HI|TI],AI),
    append(L,[HI|TI],M),
    brachylog_concatenate_(T,M,'integer':Z).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_DUPLICATES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_duplicates('string':S,'string':T) :-
    list_to_set(S,T).
brachylog_duplicates('integer':I,'integer':J) :-
    label([I]),
    number_codes(I,C),
    list_to_set(C,S),
    number_codes(J,S).
brachylog_duplicates('float':F,'float':G) :-
    number_codes(F,C),
    list_to_set(C,S),
    number_codes(G,S).
brachylog_duplicates(L,M) :-
    is_brachylog_list(L),
    list_to_set(L,M).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_ENUMERATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_enumerate('string':L,'string':[M]) :-
    nth0(_,L,M).
brachylog_enumerate(['integer':Inf,'integer':'infinite'],'integer':I) :-
    label([Inf]),
    between(Inf,infinite,I).
brachylog_enumerate(['integer':Inf,'integer':Sup],'integer':I) :-
    Sup \= 'infinite',
    Sup #>= Inf,
    label([Sup,Inf]),
    between(Inf,Sup,I).
brachylog_enumerate(['integer':Sup,'integer':Inf],'integer':I) :-
    Sup #> Inf,
    label([Sup,Inf]),
    between(Inf,Sup,N),
    I #= Inf + Sup - N.
brachylog_enumerate('integer':0,'integer':0).
brachylog_enumerate('integer':I,'integer':J) :-
    H #\= 0,
    integer_value('integer':_:[H|T],I),
    nth0(_,[H|T],M),
    integer_value('integer':'positive':[M],J).
brachylog_enumerate(L,M) :-
    L \= ['integer':_,'integer':_],
    nth0(_,L,M).

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_FINDALL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_findall(X,Y) :-
    append(X,['ignore_calling_predicate'],X2),
    findall(A,brachylog_call_predicate(X2,A),Y).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_GROUP
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_group(X,[X]).

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_HEAD
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_head('string':[H|_],'string':[H]).
brachylog_head('integer':0,'integer':0).
brachylog_head('integer':I,'integer':J) :-
    J #\= 0,
    integer_value('integer':_:[J|_],I).
brachylog_head('float':F,'integer':I) :-
    number_codes(F,L),
    brachylog_head_float(L,'integer':I).
brachylog_head([H|_],H).

brachylog_head_float([H|T],'integer':I) :-
    (
        (
            H = 48
            ;
            H = 46
        ) ->
        (
            T = [],
            I = 0
            ;
            T \= [],
            brachylog_head_float(T,'integer':I)    
        )
        ;
        H \= 48,
        H \= 46,
        number_codes(I,[H])    
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_ITERATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_iterate([Arg,'integer':I,PredName],Z) :-
    !,
    label([I]),
    brachylog_iterate_(I,PredName,Arg,Z).
brachylog_iterate(List,Z) :-
    reverse(List,[PredName,'integer':I|Arg]),
    label([I]),
    brachylog_iterate_(I,PredName,Arg,Z).

brachylog_iterate_(0,_,Z,Z).
brachylog_iterate_(I,PredName,Arg,Z) :-
    I #> 0,
    brachylog_call_predicate([Arg,PredName,'ignore_calling_predicate'],X),
    J #= I - 1,
    brachylog_iterate_(J,PredName,X,Z).

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_LENGTH
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_length([],'integer':0).
brachylog_length([H|T],'integer':Length) :-
    length([H|T],Length).
brachylog_length('string':S,'integer':Length) :-
    length(S,Length).
brachylog_length('integer':0,'integer':1).
brachylog_length('integer':I,'integer':Length) :-
    H #\= 0,
    abs(10 * I) #> Length,
    Length #>= 0,
    indomain(Length),
    length([H|T],Length),
    integer_value('integer':_:[H|T],I).
brachylog_length('float':F,'integer':Length) :-
    length(L,Length),
    catch(number_codes(F,L),E,(print_message(error,E),false)).
  
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MEMBER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_member('string':S,'string':[C]) :-
    member(C,S).
brachylog_member('integer':I,'integer':J) :-
    H #\= 0,
    integer_value('integer':_:[H|T],I),
    nth0(_,[H|T],M),
    integer_value('integer':'positive':M,J).
brachylog_member('float':F,'integer':I) :-
    number_codes(F,L),
    member(C,L),
    C \= 45,
    C \= 46,
    number_codes(I,[C]).
brachylog_member(['string':S,'integer':I],'string':[C]) :-
    length(S,L),
    L2 #= L - 1,
    I in 0..L2,
    label([I]),
    nth0(I,S,C).
brachylog_member(['integer':J,'integer':I],'integer':K) :-
    H #\= 0,
    integer_value('integer':_:[H|T],J),
    length([H|T],Length),
    Length2 #= Length - 1,
    I in 0..Length2,
    label([I]),
    nth0(I,[H|T],M),
    integer_value('integer':'positive':M,K).
brachylog_member(['float':F,'integer':I],'integer':J) :-
    number_codes(F,L),
    length(L,Length),
    Length2 #= Length - 1,
    I in 0.. Length2,
    label([I]),
    delete(L,45,L2),
    delete(L2,46,L3),
    nth0(I,L3,D),
    number_codes(J,[D]).
brachylog_member([L,'integer':I],M) :-
    is_brachylog_list(L),
    length(L,Length),
    Length2 #= Length - 1,
    I in 0.. Length2,
    label([I]),
    nth0(I,L,M).
brachylog_member([A,B,C|T],M) :-
    brachylog_member([[A,B,C|T],'integer':_],M).
  
  
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_ORDER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_order('string':S,'string':T) :-
    msort(S,T).
brachylog_order('integer':I,'integer':J) :-
    label([I]),
    number_codes(I,C),
    msort(C,D),
    number_codes(J,D).
brachylog_order(List,OrderedList) :-
    is_brachylog_list(List),
    msort(List,OrderedList).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_PERMUTE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_permute('string':S,'string':Permutation) :-
    permutation(S,Permutation).
brachylog_permute(List, Permutation) :-
    is_brachylog_list(List),
    permutation(List, Permutation).
brachylog_permute('integer':I, 'integer':J) :-
    H #\= 0,
    integer_value('integer':Sign:[H|L],I),
    permutation([H|L],M),
    integer_value('integer':Sign:M,J).
brachylog_permute('float':F, 'float':G) :-
    number_chars(F,C),
    permutation(C,D),
    \+ (D = ['.'|_]
        ;
        reverse(D,['.'|_])
    ),
    number_chars(G,D).

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_REVERSE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_reverse('string':S,'string':R) :-
    reverse(S,R).
brachylog_reverse('integer':I,'integer':R) :-
    nonvar(I),
    H #\= 0,
    A #\= 0,
    integer_value('integer':Sign:[H|T],I),
    reverse([H|T],B),
    append(Zeroes,[A|Rest],B),
    maplist(=(0),Zeroes),
    integer_value('integer':Sign:[A|Rest],R).
brachylog_reverse('integer':0,'integer':0).
brachylog_reverse('integer':I,'integer':R) :-
    var(I),
    H #\= 0,
    A #\= 0,
    integer_value('integer':Sign:[A|B],R),
    reverse(L,[A|B]),
    append(Zeroes,[H|T],L),
    maplist(=(0),Zeroes),
    integer_value('integer':Sign:[H|T],I).
brachylog_reverse(List,R) :-
    reverse(List,R).
    

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_SUBSET
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_subset('string':S,'string':T) :-
    brachylog_subset_recur(S,T).
brachylog_subset('integer':I,'integer':J) :-
    H #\= 0,
    integer_value('integer':Sign:[H|L],I),
    brachylog_subset_recur([H|L],M),
    integer_value('integer':Sign:M,J).
brachylog_subset('float':F,'float':G) :-
    number_chars(F,C),
    brachylog_subset_recur(C,D),
    \+ (D = ['.'|_]
        ;
        reverse(D,['.'|_])
    ),
    number_chars(G,D).
brachylog_subset(L,S) :-
    is_brachylog_list(L),
    brachylog_subset_recur(L,S).
    
brachylog_subset_recur([],[]).
brachylog_subset_recur([H|T],[H|T2]) :-
    brachylog_subset_recur(T,T2).
brachylog_subset_recur([_|T],T2) :-
    brachylog_subset_recur(T,T2).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_TAIL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_tail('string':L,'string':[H]) :-
    reverse(L,[H|_]).
brachylog_tail('integer':0,'integer':0).
brachylog_tail('integer':I,'integer':Z) :-
    J #\= 0,
    integer_value('integer':_:[J|T],I),
    reverse([J|T],[Z|_]).
brachylog_tail('float':F,'integer':I) :-
    number_codes(F,L),
    reverse(L,R),
    brachylog_tail_float(R,'integer':I).
brachylog_tail(L,H) :-
    is_brachylog_list(L),
    reverse(L,[H|_]).

brachylog_tail_float([H|T],'integer':I) :-
    (
        (
            H = 48
            ;
            H = 46
        ) ->
        (
            T = [],
            I = 0
            ;
            T \= [],
            brachylog_tail_float(T,'integer':I)    
        )
        ;
        H \= 48,
        H \= 46,
        number_codes(I,[H])    
    ).
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_VOID
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_void('string':[],_).
brachylog_void('integer':I,_) :-
    I #= 0.
brachylog_void('float':0.0,_).
brachylog_void([],_).    
 
 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_WRITE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_write([List,'string':F],_) :-
    is_brachylog_list(List),
    atomic_list_concat(F,Format),
    brachylog_prolog_variable(List,PrologList),
    format(Format,PrologList).
brachylog_write(Args,_) :-
    is_brachylog_list(Args),
    reverse(Args,['string':F|R]),
    reverse(R,S),
    brachylog_prolog_variable(S,PrologS),
    atomic_list_concat(F,Format),
    format(Format,PrologS).
brachylog_write('string':S,_) :-
    atomic_list_concat(S,X),
    write(X).
brachylog_write('integer':I,_) :-
    write(I).
brachylog_write('float':F,_) :-
    write(F).
brachylog_write(List,_) :-
    is_brachylog_list(List),
    \+ (reverse(List,['string':_|_])),
    brachylog_prolog_variable(List,PrologList),
    write(PrologList).
   
   
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_XTERMINATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_xterminate([L,[]],L).
brachylog_xterminate(['string':S,X],'string':T) :-
    \+ is_brachylog_list(X),
    brachylog_xterminate_(X,'string':S,'string':T).
brachylog_xterminate(['string':S,[H|T]],L3) :-
    brachylog_xterminate_(H,'string':S,L2),
    brachylog_xterminate([L2,T],L3).
brachylog_xterminate([L,H|T],L3) :-
    is_brachylog_list(L),
    \+ is_brachylog_list(H),
    brachylog_xterminate([L,[H|T]],L3).
brachylog_xterminate([L,[H|T]],L3) :-
    is_brachylog_list(L),
    delete(L,H,L2),
    brachylog_xterminate([L2,T],L3).
    
brachylog_xterminate_(X,'string':S,'string':T) :-
    brachylog_xterminate_single(X,'string':S,'string':T).
brachylog_xterminate_(_,[],[]).
brachylog_xterminate_(X,[H|T],[H2|T2]) :-
    brachylog_xterminate_single(X,H,H2),
    brachylog_xterminate_(X,T,T2).
    
brachylog_xterminate_single('string':L,'string':H,'string':Z) :-
    (
        append([A,L,B],H),
        append(A,B,H2),
        brachylog_xterminate_single('string':L,'string':H2,'string':Z)
        ;
        \+ append([_,L,_],H),
        Z = H
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_YIELD
   
   Credits to @false
   http://stackoverflow.com/a/20866206/2554145
   http://stackoverflow.com/a/11400256/2554145
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_yield('integer':I,Z) :-
    findall(X,brachylog_enumerate(['integer':0,'integer':I],X),Z).
brachylog_yield([A,B|T],Z) :-
    reverse([A,B|T],[PredName,'integer':I|RArgs]),
    reverse(RArgs,Args),
    append([Args,[PredName,'ignore_calling_predicate']],L),
    findall(X,call_firstn(brachylog_call_predicate(L,X),I),Z).

call_firstn(Goal_0, N) :-
    N + N mod 1 >= 0,         % ensures that N >=0 and N is an integer
    call_nth(Goal_0, Nth),
    ( Nth == N -> ! ; true ).

call_nth(Goal_0, C) :-
    State = count(0,_),
    Goal_0,
    arg(1, State, C1),
    C2 is C1+1,
    nb_setarg(1, State, C2),
    C = C2.
    

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_ZIP
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_zip(L,Z) :-
    is_brachylog_list(L),
    maplist(brachylog_length,L,Lengths),
    brachylog_order(Lengths,OrderedLengths),
    reverse(OrderedLengths,['integer':MaxLength|_]),
    brachylog_zip_(L,MaxLength,Z).
    
brachylog_zip_(_,0,[]).
brachylog_zip_(L,I,[Heads|Z]) :-
    I #> 0,
    maplist(brachylog_head,L,Heads),
    maplist(brachylog_math_circular_permute_left,L,Tails),
    J #= I - 1,
    brachylog_zip_(Tails,J,Z).

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_CALL_PREDICATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
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

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_PLUS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_plus('integer':I,'integer':AbsoluteValue) :-
    AbsoluteValue #= abs(I).
brachylog_plus('float':F,'float':AbsoluteValue) :-
    AbsoluteValue is abs(F).
brachylog_plus(L,Sum) :-
    is_brachylog_list(L),
    \+ (maplist(is_brachylog_list,L)),
    brachylog_plus_(L,Sum).
brachylog_plus(ListOfLists,Sums) :-
    maplist(is_brachylog_list,ListOfLists),
    ListOfLists = [H|_],
    length(H,Length),
    (
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
 
 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MINUS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_minus('integer':I,'integer':J) :-
    J #= -I,
    label([I,J]).
brachylog_minus('float':I,'float':J) :-
    J is -I.
brachylog_minus([],'integer':[0]).
brachylog_minus(['integer':I1,'integer':I2],'integer':Sum) :-
    Sum #= I1 - I2,
    label([Sum,I1,I2]).
brachylog_minus(['float':I1,'integer':I2],'float':Sum) :-
    label([I2]),
    nonvar(I1),
    Sum is I1 - I2.
brachylog_minus(['integer':I1,'float':I2],'float':Sum) :-
    label([I1]),
    nonvar(I2),
    Sum is I1 - I2.
brachylog_minus(['float':I1,'float':I2],'float':Sum) :-
    nonvar(I1),
    nonvar(I2),
    Sum is I1 - I2.

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MULTIPLY
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_multiply('integer':I,'integer':J) :-
    J #= 2*I.
brachylog_multiply('float':F,'float':G) :-
    G is 2*F.
brachylog_multiply(L,Product) :-
	is_brachylog_list(L),
	\+ (maplist(is_brachylog_list,L)),
	brachylog_multiply_(L,Product).
brachylog_multiply(ListOfLists,Products) :-
	maplist(is_brachylog_list,ListOfLists),
	ListOfLists = [H|_],
	length(H,Length),
	(
		maplist(length_(Length),ListOfLists),
		transpose(ListOfLists,Transpose),
		maplist(brachylog_multiply_,Transpose,Products)
		;
		\+ (maplist(length_(Length),ListOfLists)),
		throw('Lists must have have the same length to be multiplied')
	).
	
brachylog_multiply_([],'integer':1).
brachylog_multiply_([TypeI:I|T],TypeS:Product) :-
	brachylog_multiply_(T,TypeF:F),
	(
		TypeI = 'integer',
		TypeF = 'integer',
		Product #= I * F,
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
		Product is I * F
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_DIVIDE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_divide('integer':I,'float':J) :-
	label([I]),
	J is 1/I.
brachylog_divide('float':I,'float':J) :-
	J is 1/I.
brachylog_divide([],'integer':1).
brachylog_divide(['integer':I1,'integer':I2],'integer':Division) :-
	Division #= I1 // I2,
	label([Division,I1,I2]).
brachylog_divide(['float':I1,'integer':I2],'float':Division) :-
	label([I2]),
	nonvar(I1),
	Division is I1 / I2.
brachylog_divide(['integer':I1,'float':I2],'float':Division) :-
	label([I1]),
	nonvar(I2),
	Division is I1 / I2.
brachylog_divide(['float':I1,'float':I2],'float':Division) :-
	nonvar(I1),
	nonvar(I2),
	Division is I1 / I2.

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_POWER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_power('integer':I,'integer':J) :-
    J #= I^2.
brachylog_power('float':I,'float':J) :-
    J is I^2.
brachylog_power([],'integer':1).
brachylog_power(['integer':I1,'integer':I2],'integer':Power) :-
    Power #= I1 ^ I2,
    label([Power,I1,I2]).
brachylog_power(['float':I1,'integer':I2],'float':Power) :-
    label([I2]),
    nonvar(I1),
    Power is I1 ^ I2.
brachylog_power(['integer':I1,'float':I2],'float':Power) :-
    label([I1]),
    nonvar(I2),
    Power is I1 ^ I2.
brachylog_power(['float':I1,'float':I2],'float':Power) :-
    nonvar(I1),
    nonvar(I2),
    Power is I1 ^ I2.

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_LESS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_less('integer':I1,'integer':I2) :-
    I1 #< I2.
brachylog_less('float':I1,'integer':I2) :-
    label([I2]),
    nonvar(I1),
    I1 < I2.
brachylog_less('integer':I1,'float':I2) :-
    label([I1]),
    nonvar(I2),
    I1 < I2.
brachylog_less('float':I1,'float':I2) :-
    nonvar(I1),
    nonvar(I2),
    I1 < I2.
    

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_GREATER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_greater('integer':I1,'integer':I2) :-
    I1 #> I2.
brachylog_greater('float':I1,'integer':I2) :-
    label([I2]),
    nonvar(I1),
    I1 > I2.
brachylog_greater('integer':I1,'float':I2) :-
    label([I1]),
    nonvar(I2),
    I1 > I2.
brachylog_greater('float':I1,'float':I2) :-
    nonvar(I1),
    nonvar(I2),
    I1 > I2.    
   
   
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_LESSEQUAL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_lessequal('integer':I1,'integer':I2) :-
    I1 #=< I2.
brachylog_lessequal('float':I1,'integer':I2) :-
    label([I2]),
    nonvar(I1),
    I1 =< I2.
brachylog_lessequal('integer':I1,'float':I2) :-
    label([I1]),
    nonvar(I2),
    I1 =< I2.
brachylog_lessequal('float':I1,'float':I2) :-
    nonvar(I1),
    nonvar(I2),
    I1 =< I2.
     
     
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_GREATEREQUAL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_greaterequal('integer':I1,'integer':I2) :-
    I1 #>= I2.
brachylog_greaterequal('float':I1,'integer':I2) :-
    label([I2]),
    nonvar(I1),
    I1 >= I2.
brachylog_greaterequal('integer':I1,'float':I2) :-
    label([I1]),
    nonvar(I2),
    I1 >= I2.
brachylog_greaterequal('float':I1,'float':I2) :-
    nonvar(I1),
    nonvar(I2),
    I1 >= I2.
    

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_EQUALS
   
   Credits to Markus Triska
   See: http://codereview.stackexchange.com/questions/129924/clpfd-labeling-on-possibly-infinite-domains/129945#129945
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_equals('integer':Z,'integer':Z) :-
    unsafe_indomain(Z).

brachylog_equals(Z,Z) :-
	is_brachylog_list(Z),
	maplist(brachylog_equals,Z,_).

unsafe_indomain(X) :-
    fd_inf(X, Inf0),
    fd_sup(X, Sup0),
    maplist(limit_pure, [Inf0,Sup0], [Inf,Sup]),
    unsafe_indomain_(Inf, Sup, X).

limit_pure(inf, inf) :- !.
limit_pure(sup, sup) :- !.
limit_pure(N, n(N))  :- !.

unsafe_indomain_(inf, Sup, X) :-
    infinite_down(Sup, X).
unsafe_indomain_(n(Low), Sup, X) :-
    unsafe_up_(Sup, Low, X).

infinite_down(sup, X) :-
    (   X = 0
    ;   positive_integer(N),
        ( X #= N ; X #= -N )
    ).
infinite_down(n(Up), X ) :-
    (   between(0, Up, X)
    ;   length([_|_], N),
        X #= -N
    ).

unsafe_up_(sup, Low, X) :-
    (   between(Low, 0, X)
    ;   positive_integer(X)
    ).
unsafe_up_(n(Up), Low, X) :- between(Low, Up, X).

positive_integer(N) :- length([_|_], N).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MODULO
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_modulo(['integer':I1,'integer':I2],'integer':Rem) :-
    Rem #= I1 mod I2,
    label([I1,I2,Rem]).
