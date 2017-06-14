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


:- module(metapredicates, [brachylog_meta_accumulate/5,
                           brachylog_meta_count/5,
                           brachylog_meta_declare/5,
                           brachylog_meta_find/5,
                           brachylog_meta_groupby/5,
                           brachylog_meta_iterate/5,
                           brachylog_meta_leftfold/5,
                           brachylog_meta_map/5,
                           brachylog_meta_orderby/5,
                           brachylog_meta_select/5,
                           brachylog_meta_unique/5,
                           brachylog_meta_zip/5
                          ]).

:- use_module(library(clpfd)).
:- use_module(predicates).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_ACCUMULATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_accumulate('first', P, Sub, ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_accumulate('integer':I, P, Sub, Arg, Output).
brachylog_meta_accumulate('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_accumulate('integer':I, P, Sub, Arg, Output).
brachylog_meta_accumulate('default', P, Sub, Input, Output) :-
    brachylog_meta_accumulate('integer':1, P, Sub, Input, Output).
brachylog_meta_accumulate(Sup, P, Sub, 'string':Input, Output) :-
    brachylog_meta_accumulate(Sup, P, Sub, ['string':Input], Output).
brachylog_meta_accumulate(Sup, P, Sub, 'integer':Input, Output) :-
    brachylog_meta_accumulate(Sup, P, Sub, ['integer':Input], Output).
brachylog_meta_accumulate(Sup, P, Sub, 'float':Input, Output) :-
    brachylog_meta_accumulate(Sup, P, Sub, ['float':Input], Output).
brachylog_meta_accumulate('integer':0, _P, _Sub, Input, Input).
brachylog_meta_accumulate('integer':I, P, Sub, Input, Output) :-
    I #> 0,
    is_brachylog_list(Input),
    call(P, Sub, Input, E),
    J #= I - 1,
    append(Input, [E], F),
    brachylog_meta_accumulate('integer':J, P, Sub, F, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_COUNT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_count('first', P, Sub, ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_count('integer':I, P, Sub, Arg, Output).
brachylog_meta_count('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_count('integer':I, P, Sub, Arg, Output).
brachylog_meta_count('default', P, Sub, Input, Output) :-
    brachylog_meta_count('integer':0, P, Sub, Input, Output).
brachylog_meta_count('integer':0, P, Sub, Input, Output) :-
    brachylog_meta_find('default', P, Sub, Input, E),
    brachylog_length('default', E, Output).
brachylog_meta_count('integer':1, P, Sub, Input, Output) :-
    brachylog_meta_unique('default', P, Sub, Input, E),
    brachylog_length('default', E, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_DECLARE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_declare('first', P, Sub, ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_declare('integer':I, P, Sub, Arg, Output).
brachylog_meta_declare('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_declare('integer':I, P, Sub, Arg, Output).
brachylog_meta_declare('default', P, Sub, [H,T], T) :-
    call(P, Sub, H, T).
brachylog_meta_declare('integer':0, P, Sub, [H,T], [H,T]) :-
    call(P, Sub, H, T).
brachylog_meta_declare('integer':1, P, Sub, [H,T], H) :-
    call(P, Sub, T, H).
brachylog_meta_declare('integer':2, P, Sub, [H,T], [H,T]) :-
    call(P, Sub, T, H).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_FIND
   
   Credits to @false for call_firstf/2 and call_nth/2
   http://stackoverflow.com/a/20866206/2554145
   http://stackoverflow.com/a/11400256/2554145
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_find('first', P, Sub, ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_find('integer':I, P, Sub, Arg, Output).
brachylog_meta_find('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_find('integer':I, P, Sub, Arg, Output).
brachylog_meta_find('integer':0, _, _, _, []).
brachylog_meta_find('default', P, Sub, Input, Output) :-
    findall(X, call(P, Sub, Input, X), Output).
brachylog_meta_find('integer':I, P, Sub, Input, Output) :-
    I #> 0,
    findall(X, call_firstn(call(P, Sub, Input, X), I), Output).

call_firstn(Goal_0, N) :-
    N + N mod 1 >= 0,         % ensures that N >=0 and N is an integer
    call_nth(Goal_0, Nth),
    ( Nth == N -> ! ; true ).

call_nth(Goal_0, C) :-
    State = count(0, _),
    Goal_0,
    arg(1, State, C1),
    C2 is C1+1,
    nb_setarg(1, State, C2),
    C = C2.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_GROUPBY
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_groupby('first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_groupby('integer':I, P, Sub, Arg, Output).
brachylog_meta_groupby('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_groupby('integer':I, P, Sub, Arg, Output).
brachylog_meta_groupby('default', P, Sub, Input, Output) :-
    (   is_brachylog_list(Input) -> FixedInput = Input
    ;   brachylog_elements('default', Input, FixedInput)
    ),
    brachylog_meta_map('default', P, Sub, FixedInput, L),
    brachylog_zip('default', [L,Input], L2),
    brachylog_meta_groupby_group(L2, L3),
    maplist(brachylog_meta_groupby_tail, L3, Output).

brachylog_meta_groupby_group(L, Gs) :-
    brachylog_meta_groupby_group(L, [], Gs).

brachylog_meta_groupby_group([], Gs, Gs).
brachylog_meta_groupby_group([[G,H]|T], TempGs, Gs) :-
    (   member(G:L, TempGs) ->
        reverse(L, RL),
        reverse([H|RL], L2),
        select(G:L, TempGs, G:L2, TempGs2)
    ;   append(TempGs, [G:[H]], TempGs2)
    ),
    brachylog_meta_groupby_group(T, TempGs2, Gs).

brachylog_meta_groupby_tail(_:T, T).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_ITERATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_iterate('first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_iterate('integer':I, P, Sub, Arg, Output).
brachylog_meta_iterate('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_iterate('integer':I, P, Sub, Arg, Output).
brachylog_meta_iterate('integer':0, _, _, Input, Input).
brachylog_meta_iterate('default', P, Sub, Input, Output) :-
    brachylog_meta_iterate('integer':1, P, Sub, Input, Output).
brachylog_meta_iterate('integer':1, P, Sub, Input, Output) :-
    call(P, Sub, Input, Output).
brachylog_meta_iterate('integer':I, P, Sub, Input, Output) :-
    I #> 1,
    call(P, Sub, Input, TOutput),
    J #= I - 1,
    brachylog_meta_iterate('integer':J, P, Sub, TOutput, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_LEFTFOLD
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_leftfold('first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_leftfold('integer':I, P, Sub, Arg, Output).
brachylog_meta_leftfold('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_leftfold('integer':I, P, Sub, Arg, Output).
brachylog_meta_leftfold('default', P, Sub, 'string':S, Output) :-
    brachylog_elements('default', 'string':S, E),
    brachylog_meta_leftfold('default', P, Sub, E, O),
    (   brachylog_concatenate('default', O, X),
        X = 'string':_ ->
        Output = X
    ;   Output = O
    ).
brachylog_meta_leftfold('default', P, Sub, 'integer':Input, Output) :-
    brachylog_elements('default', 'integer':Input, E),
    brachylog_meta_leftfold('default', P, Sub, E, O),
    (   brachylog_concatenate('default', O, X),
        X = 'integer':_ ->
        Output = X
    ;   Output = O
    ).
brachylog_meta_leftfold('default', _P, _Sub, [], []).
brachylog_meta_leftfold('default', _P, _Sub, [X], [X]).
brachylog_meta_leftfold('default', P, Sub, [H,I|T], Output) :-
    brachylog_meta_leftfold_(P, Sub, [I|T], H, Output).

brachylog_meta_leftfold_(_P, _Sub, [], Output, Output).
brachylog_meta_leftfold_(P, Sub, [H|T], A, Output) :-
    call(P, Sub, [A,H], E),
    brachylog_meta_leftfold_(P, Sub, T, E, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_MAP
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_map('first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_map('integer':I, P, Sub, Arg, Output).
brachylog_meta_map('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_map('integer':I, P, Sub, Arg, Output).
brachylog_meta_map('integer':0, P, Sub, Input, Output) :-
    call(P, Sub, Input, Output).
brachylog_meta_map('default', P, Sub, Input, Output) :-
    brachylog_meta_map('integer':1, P, Sub, Input, Output).
brachylog_meta_map('integer':I, P, Sub, 'string':S, Output) :-
    brachylog_elements('default', 'string':S, E),
    brachylog_meta_map('integer':I, P, Sub, E, O),
    (   brachylog_concatenate('default', O, X),
        X = 'string':_ ->
        Output = X
    ;   Output = O
    ).
brachylog_meta_map('integer':I, P, Sub, 'integer':Input, Output) :-
    brachylog_elements('default', 'integer':Input, E),
    brachylog_meta_map('integer':I, P, Sub, E, O),
    (   brachylog_concatenate('default', O, X),
        X = 'integer':_ ->
        Output = X
    ;   Output = O
    ).
brachylog_meta_map('integer':1, P, Sub, Input, Output) :-
    Pred =.. [P, Sub],
    is_brachylog_list(Input),
    maplist(Pred, Input, Output).
brachylog_meta_map('integer':I, P, Sub, Input, Output) :-
    I #> 1,
    J #= I - 1,
    is_brachylog_list(Input),
    maplist(brachylog_meta_map('integer':J, P, Sub), Input, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_ORDERBY
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_orderby('first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_orderby('integer':I, P, Sub, Arg, Output).
brachylog_meta_orderby('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_orderby('integer':I, P, Sub, Arg, Output).
brachylog_meta_orderby('default', P, Sub, Input, Output) :-
    brachylog_meta_orderby('integer':0, P, Sub, Input, Output).
brachylog_meta_orderby('integer':0, P, Sub, Input, Output) :-
    (   is_brachylog_list(Input) -> FixedInput = Input
    ;   brachylog_elements('default', Input, FixedInput)
    ),
    brachylog_meta_map('default', P, Sub, FixedInput, L),
    brachylog_zip('default', [L,Input], L2),
    msort(L2, SL2),
    (   SL2 = [] ->
        Output = []
    ;   brachylog_zip('default', SL2, [_,Output])
    ).
brachylog_meta_orderby('integer':1, P, Sub, Input, Output) :-
    (   is_brachylog_list(Input) -> FixedInput = Input
    ;   brachylog_elements('default', Input, FixedInput)
    ),
    brachylog_meta_map('default', P, Sub, FixedInput, L),
    brachylog_zip('default', [L,Input], L2),
    msort(L2, RSL2),
    reverse(RSL2, SL2),
    (   SL2 = [] ->
        Output = []
    ;   brachylog_zip('default', SL2, [_,Output])
    ).
brachylog_meta_orderby('integer':2, P, Sub, Input, Output) :-
    (   is_brachylog_list(Input) -> FixedInput = Input
    ;   brachylog_elements('default', Input, FixedInput)
    ),
    brachylog_meta_map('default', P, Sub, FixedInput, L),
    msort(L, Output).
brachylog_meta_orderby('integer':3, P, Sub, Input, Output) :-
    (   is_brachylog_list(Input) -> FixedInput = Input
    ;   brachylog_elements('default', Input, FixedInput)
    ),
    brachylog_meta_map('default', P, Sub, FixedInput, L),
    msort(L, RL),
    reverse(RL, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_SELECT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_select('first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_select('integer':I, P, Sub, Arg, Output).
brachylog_meta_select('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_select('integer':I, P, Sub, Arg, Output).
brachylog_meta_select('default', P, Sub, 'string':S, Output) :-
    brachylog_elements('default', 'string':S, E),
    brachylog_meta_select('default', P, Sub, E, O),
    (   brachylog_concatenate('default', O, X),
        X = 'string':_ ->
        Output = X
    ;   Output = O
    ).
brachylog_meta_select('default', P, Sub, 'integer':S, Output) :-
    brachylog_elements('default', 'integer':S, E),
    brachylog_meta_select('default', P, Sub, E, O),
    (   brachylog_concatenate('default', O, X),
        X = 'integer':_ ->
        Output = X
    ;   Output = O
    ).
brachylog_meta_select('default', _, _, [], []).
brachylog_meta_select('default', P, Sub, [H|T], Output) :-
    (   call(P, Sub, H, H2) *->
        Output = [H2|T2]
    ;   Output = T2
    ),
    brachylog_meta_select('default', P, Sub, T, T2).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_UNIQUE
   
   TODO: fix infinite loops when the subscript is bigger than the number of
         existing answers. The between call generates infinite choice 
         points which is bad if length(Output, I) is impossible.
   
   TODO: improve performance outside default mod. Right now it recomputes 
         all answers until it finds a set of unique answers, instead of
         keeping the ones it already found.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_unique('first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_unique('integer':I, P, Sub, Arg, Output).
brachylog_meta_unique('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_unique('integer':I, P, Sub, Arg, Output).
brachylog_meta_unique('default', P, Sub, Input, Output) :-
    setof(X, call(P, Sub, Input, X), Output).
brachylog_meta_unique('integer':0, _, _, _, []).
brachylog_meta_unique('integer':I, P, Sub, Input, Output) :-
    I #> 0,
    length(Output, I),
    between(I, infinite, J),
    setof(X, call_firstn(call(P, Sub, Input, X), J), Output),
    !.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_ZIP
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_zip('first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_zip('integer':I, P, Sub, Arg, Output).
brachylog_meta_zip('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_zip('integer':I, P, Sub, Arg, Output).
brachylog_meta_zip('default', P, Sub, Arg, Output) :-
    brachylog_meta_map('default', P, Sub, Arg, O),
    brachylog_zip('default', [Arg,O], Output).
brachylog_meta_zip('integer':0, P, Sub, Arg, Output) :-
    brachylog_meta_find('default', P, Sub, Arg, O),
    brachylog_zip('default', [Arg,O], Output).
brachylog_meta_zip('integer':1, P, Sub, Arg, Output) :-
    brachylog_meta_map('default', P, Sub, Arg, O),
    brachylog_zip('default', [O,Arg], Output).
brachylog_meta_zip('integer':2, P, Sub, Arg, Output) :-
    brachylog_meta_find('default', P, Sub, Arg, O),
    brachylog_zip('default', [O,Arg], Output).

