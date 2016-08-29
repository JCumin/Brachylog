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


:- module(string_predicates, [brachylog_string_blocks/2,
                              brachylog_string_to_codes/2,
                              brachylog_string_elements/2,
                              brachylog_string_lowercase/2,
                              brachylog_string_split_lines/2,
                              brachylog_string_pad/2,
                              brachylog_string_uppercase/2,
                              brachylog_string_writeln/2,
                              brachylog_string_dichotomize/2,
                              brachylog_string_trichotomize/2,
                              brachylog_string_tetrachotomize/2,
                              brachylog_string_pentachotomize/2,
                              brachylog_string_hexachotomize/2,
                              brachylog_string_heptachotomize/2,
                              brachylog_string_octachotomize/2,
                              brachylog_string_enneachotomize/2,
                              brachylog_string_to_number/2,
                              brachylog_string_random_element/2,
                              brachylog_string_shuffle/2
                             ]).
                       
:- use_module(library(clpfd)).
:- use_module(predicates).
:- use_module(utils).

 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_STRING_BLOCKS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_string_blocks([],[]).
brachylog_string_blocks([H|T],Blocks) :-
    brachylog_string_blocks([H|T],H,Blocks).
brachylog_string_blocks('string':[H|T],StringBlocks) :-
    maplist(prepend_string,Blocks,StringBlocks),
    brachylog_string_blocks([H|T],H,Blocks),
    !.

brachylog_string_blocks([],_,[[]]).
brachylog_string_blocks([H|T],H,[[H|T2]|T3]) :-
    brachylog_string_blocks(T,H,[T2|T3]).
brachylog_string_blocks([H|T],I,[[],[H|T2]|T3]) :-
    dif(H,I),
    brachylog_string_blocks(T,H,[T2|T3]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_STRING_TO_CODES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_string_to_codes('string':[],[]).
brachylog_string_to_codes('string':[H|T],['integer':I|T2]) :-
    (
        var(T)
        -> length(T2,Length),
        length(T,Length)
        ;
        length(T,Length),
        length(T2,Length)
    ),
    maplist(prepend_integer,L,['integer':I|T2]),
    maplist(single_atom_code,[H|T],L).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_STRING_ELEMENTS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_string_elements([],[]).
brachylog_string_elements([H|T],L) :-
    maplist(brachylog_string_elements,[H|T],L).
brachylog_string_elements('string':S,L) :-
    brachylog_findall(['string':S,brachylog_enumerate],L).
brachylog_string_elements('integer':I,L) :-
    brachylog_findall(['integer':I,brachylog_enumerate],L).    


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_STRING_LOWERCASE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_string_lowercase('string':Ls0,'string':Ls) :-
    maplist(downcase_atom, Ls0, Ls).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_STRING_SPLIT_LINES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_string_split_lines('string':[],['string':[]]).
brachylog_string_split_lines('string':['\n'|T],['string':[]|T3]) :-
    brachylog_string_split_lines('string':T,T3).
brachylog_string_split_lines('string':['\r','\r\n'|T],['string':[]|T3]) :-
    brachylog_string_split_lines('string':T,T3).
brachylog_string_split_lines('string':[H|T],['string':[H|T2]|T3]) :-
    dif(H,'\n'),
    dif(H,'\r\n'),
    brachylog_string_split_lines('string':T,['string':T2|T3]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_STRING_PAD
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_string_pad([],[]).
brachylog_string_pad([H|T],Padded) :-
    length([H|T],L),
    length(Padded,L),
    maplist(brachylog_length,[H|T],Lengths),
    brachylog_order(Lengths,OrderedLengths),
    reverse(OrderedLengths,['integer':MaxLength|_]),
    maplist(brachylog_string_pad_(MaxLength),[H|T],Padded).

brachylog_string_pad_(MaxL,'string':S,'string':Z) :-
    length(Z,MaxL),
    length(S,L),
    L2 #= MaxL - L,
    length(T,L2),
    maplist(=(' '),T),
    brachylog_concatenate(['string':S,'string':T],'string':Z).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_STRING_UPPERCASE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_string_uppercase('string':Ls0,'string':Ls) :-
    maplist(upcase_atom, Ls0, Ls).
    

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_STRING_WRITELN
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_string_writeln(X,_) :-
    brachylog_write(X,_),
    brachylog_write('string':['\n'],_).

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_STRING_NCHOTOMIZE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_string_Nchotomize(L,L2) :-
    is_brachylog_list(L),
    reverse(L,['integer':I|RArgs]),
    reverse(RArgs,Args),
    (
        Args = [A]
        -> RealArgs = A
        ;
        RealArgs = Args
    ),
    brachylog_equals('integer':I,_),
    length(L2,I),
    brachylog_string_Nchotomize_(RealArgs,L2).

brachylog_string_dichotomize(X,[A,B]) :-
    brachylog_string_Nchotomize_(X,[A,B]).

brachylog_string_trichotomize(X,[A,B,C]) :-
    brachylog_string_Nchotomize_(X,[A,B,C]).

brachylog_string_tetrachotomize(X,[A,B,C,D]) :-
    brachylog_string_Nchotomize_(X,[A,B,C,D]).

brachylog_string_pentachotomize(X,[A,B,C,D,E]) :-
    brachylog_string_Nchotomize_(X,[A,B,C,D,E]).

brachylog_string_hexachotomize(X,[A,B,C,D,E,F]) :-
    brachylog_string_Nchotomize_(X,[A,B,C,D,E,F]).

brachylog_string_heptachotomize(X,[A,B,C,D,E,F,G]) :-
    brachylog_string_Nchotomize_(X,[A,B,C,D,E,F,G]).

brachylog_string_octachotomize(X,[A,B,C,D,E,F,G,H]) :-
    brachylog_string_Nchotomize_(X,[A,B,C,D,E,F,G,H]).

brachylog_string_enneachotomize(X,[A,B,C,D,E,F,G,H,I]) :-
    brachylog_string_Nchotomize_(X,[A,B,C,D,E,F,G,H,I]).
    
brachylog_string_Nchotomize_('string':L,L2) :-
    maplist(prepend_string,M,L2),
    brachylog_string_Nchotomize_(L,M).
brachylog_string_Nchotomize_(L,L2) :-
    is_brachylog_list(L),
    maplist(is_brachylog_list,L2),
    Length #= LengthL//LengthL2,
    length(L2,LengthL2),
    length(L,LengthL),
    reverse(L2,[_|T]),
    maplist(length_(Length),T),
    append(L2,L),
    !.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_STRING_TO_NUMBER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_string_to_number('string':S,Type:N) :-
    atomic_list_concat(S,A),
    atom_number(A,N),
    (
        member('.',S)
        -> Type = 'float'
        ;
        Type = 'integer'
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_STRING_RANDOM_ELEMENT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_string_random_element([],[]).
brachylog_string_random_element([H|T],R) :-
    length([H|T],L),
    M #= L - 1,
    random_between(0,M,I),
    nth0(I,[H|T],R).
brachylog_string_random_element('string':S,'string':R) :-
    brachylog_string_random_element(S,R).
brachylog_string_random_element('integer':0,'integer':0).
brachylog_string_random_element('integer':I,'integer':R) :-
    H #\= 0,
    integer_value('integer':_:[H|T],I),
    brachylog_string_random_element([H|T],R).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_STRING_SHUFFLE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_string_shuffle([],[]).
brachylog_string_shuffle([H|T],[H2|T2]) :-
    random_permutation([H|T],[H2|T2]).
brachylog_string_shuffle('string':S,'string':T) :-
    brachylog_string_shuffle(S,T).
brachylog_string_shuffle('integer':I,'integer':J) :-
    H #\= 0,
    integer_value('integer':Sign:[H|T],I),
    (
        H2 #\= 0,
        brachylog_string_shuffle([H|T],[H2|T2])
        -> integer_value('integer':Sign:[H2|T2],J)
        ;
        brachylog_string_shuffle('integer':I,'integer':J)
    ).
