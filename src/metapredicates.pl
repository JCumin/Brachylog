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


:- module(metapredicates, [brachylog_meta_accumulate/6,
                           brachylog_meta_bagof/6,
                           brachylog_meta_count/6,
                           brachylog_meta_declare/6,
                           brachylog_meta_existence/6,
                           brachylog_meta_find/6,
                           brachylog_meta_groupby/6,
                           brachylog_meta_head/6,
                           brachylog_meta_iterate/6,
                           brachylog_meta_leftfold/6,
                           brachylog_meta_map/6,
                           brachylog_meta_nonexistence/6,
                           brachylog_meta_orderby/6,
                           brachylog_meta_select/6,
                           brachylog_meta_tail/6,
                           brachylog_meta_unique/6,
                           brachylog_meta_verify/6,
                           brachylog_meta_zip/6
                          ]).

:- use_module(library(clpfd)).
:- use_module(predicates).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_ACCUMULATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_accumulate(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_accumulate(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_accumulate(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_accumulate(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_accumulate(GlobalVariables, 'default', P, Sub, Input, Output) :-
    brachylog_meta_accumulate(GlobalVariables, 'integer':1, P, Sub, Input, Output).
brachylog_meta_accumulate(GlobalVariables, Sup, P, Sub, 'string':Input, Output) :-
    brachylog_meta_accumulate(GlobalVariables, Sup, P, Sub, ['string':Input], Output).
brachylog_meta_accumulate(GlobalVariables, Sup, P, Sub, 'integer':Input, Output) :-
    brachylog_meta_accumulate(GlobalVariables, Sup, P, Sub, ['integer':Input], Output).
brachylog_meta_accumulate(GlobalVariables, Sup, P, Sub, 'float':Input, Output) :-
    brachylog_meta_accumulate(GlobalVariables, Sup, P, Sub, ['float':Input], Output).
brachylog_meta_accumulate(_, 'integer':0, _P, _Sub, Input, Input).
brachylog_meta_accumulate(GlobalVariables, 'integer':I, P, Sub, Input, Output) :-
    I #> 0,
    is_brachylog_list(Input),
    (   GlobalVariables = 'ignore',
        call(P, Sub, Input, E)
    ;   dif(GlobalVariables, 'ignore'),
        call(P, GlobalVariables, Sub, Input, E)
    ),
    J #= I - 1,
    append(Input, [E], F),
    brachylog_meta_accumulate(GlobalVariables, 'integer':J, P, Sub, F, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_BAGOF
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_bagof(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_bagof(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_bagof(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_bagof(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_bagof(_, 'integer':0, _, _, _, []).
brachylog_meta_bagof(GlobalVariables, 'default', P, Sub, Input, Output) :-
    bagof(X,
        (   GlobalVariables = 'ignore',
            call(P, Sub, Input, X)
        ;   dif(GlobalVariables, 'ignore'),
            call(P, GlobalVariables, Sub, Input, X)
        ),
        Output).
brachylog_meta_bagof(GlobalVariables, 'integer':I, P, Sub, Input, Output) :-
    I #> 0,
    bagof(X, call_firstn(
        (   GlobalVariables = 'ignore',
            call(P, Sub, Input, X)
        ;   dif(GlobalVariables, 'ignore'),
            call(P, GlobalVariables, Sub, Input, X)
        ), I),
        Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_COUNT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_count(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_count(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_count(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_count(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_count(GlobalVariables, 'default', P, Sub, Input, Output) :-
    brachylog_meta_count(GlobalVariables, 'integer':0, P, Sub, Input, Output).
brachylog_meta_count(GlobalVariables, 'integer':0, P, Sub, Input, Output) :-
    brachylog_meta_find(GlobalVariables, 'default', P, Sub, Input, E),
    brachylog_length('default', E, Output).
brachylog_meta_count(GlobalVariables, 'integer':1, P, Sub, Input, Output) :-
    brachylog_meta_unique(GlobalVariables, 'default', P, Sub, Input, E),
    brachylog_length('default', E, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_DECLARE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_declare(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_declare(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_declare(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_declare(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_declare(GlobalVariables, 'default', P, Sub, [H,T], T) :-
    (   GlobalVariables = 'ignore',
        call(P, Sub, H, T)
    ;   dif(GlobalVariables, 'ignore'),
        call(P, GlobalVariables, Sub, H, T)
    ).
brachylog_meta_declare(GlobalVariables, 'integer':0, P, Sub, [H,T], [H,T]) :-
    (   GlobalVariables = 'ignore',
        call(P, Sub, H, T)
    ;   dif(GlobalVariables, 'ignore'),
        call(P, GlobalVariables, Sub, H, T)
    ).
brachylog_meta_declare(GlobalVariables, 'integer':1, P, Sub, [H,T], H) :-
    (   GlobalVariables = 'ignore',
        call(P, Sub, T, H)
    ;   dif(GlobalVariables, 'ignore'),
        call(P, GlobalVariables, Sub, T, H)
    ).
brachylog_meta_declare(GlobalVariables, 'integer':2, P, Sub, [H,T], [H,T]) :-
    (   GlobalVariables = 'ignore',
        call(P, Sub, T, H)
    ;   dif(GlobalVariables, 'ignore'),
        call(P, GlobalVariables, Sub, T, H)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_EXISTENCE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_existence(GlobalVariables, 'first', P, Sub, [I|Input], Arg) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_existence(GlobalVariables, P, Sub, Arg, I).
brachylog_meta_existence(GlobalVariables, 'last', P, Sub, Input, Arg) :-
    reverse(Input, [I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_existence(GlobalVariables, P, Sub, Arg, I).
brachylog_meta_existence(GlobalVariables, 'integer':I, P, Sub, Input, Input) :-
    dif(I, 'default'),
    brachylog_meta_existence(GlobalVariables, P, Sub, Input, 'integer':I).
brachylog_meta_existence(GlobalVariables, 'default', P, Sub, Input, Output) :-
    brachylog_meta_existence(GlobalVariables, P, Sub, Input, Output).

brachylog_meta_existence(GlobalVariables, P, Sub, Input, Output) :-
    brachylog_in('default', Input, E),
    (   GlobalVariables = 'ignore',
        call(P, Sub, E, Output)
    ;   dif(GlobalVariables, 'ignore'),
        call(P, GlobalVariables, Sub, E, Output)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_FIND

   Credits to @false for call_firstf/2 and call_nth/2
   http://stackoverflow.com/a/20866206/2554145
   http://stackoverflow.com/a/11400256/2554145
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_find(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_find(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_find(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_find(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_find(_, 'integer':0, _, _, _, []).
brachylog_meta_find(GlobalVariables, 'default', P, Sub, Input, Output) :-
    findall(X,
        (   GlobalVariables = 'ignore',
            call(P, Sub, Input, X)
        ;   dif(GlobalVariables, 'ignore'),
            call(P, GlobalVariables, Sub, Input, X)
        ),
        Output).
brachylog_meta_find(GlobalVariables, 'integer':I, P, Sub, Input, Output) :-
    I #> 0,
    findall(X, call_firstn(
        (   GlobalVariables = 'ignore',
            call(P, Sub, Input, X)
        ;   dif(GlobalVariables, 'ignore'),
            call(P, GlobalVariables, Sub, Input, X)
        ), I),
        Output).

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
brachylog_meta_groupby(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_groupby(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_groupby(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_groupby(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_groupby(GlobalVariables, 'default', P, Sub, Input, Output) :-
    (   is_brachylog_list(Input) -> FixedInput = Input
    ;   brachylog_elements('default', Input, FixedInput)
    ),
    brachylog_meta_map(GlobalVariables, 'default', P, Sub, FixedInput, L),
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
   BRACHYLOG_META_HEAD
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_head(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_head(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_head(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_head(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_head(GlobalVariables, 'default', P, Sub, Input, Output) :-
    brachylog_meta_head(GlobalVariables, 'integer':1, P, Sub, Input, Output).
brachylog_meta_head(_, 'integer':0, _, _, Input, Input).
brachylog_meta_head(GlobalVariables, 'integer':I, P, Sub, Input, Output) :-
    I #> 0,
    brachylog_head('integer':I, Input, Heads),
    brachylog_behead('integer':I, Input, Tails),
    brachylog_meta_map(GlobalVariables, 'default', P, Sub, Heads, NewHeads),
    brachylog_concatenate('default', [NewHeads,Tails], Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_ITERATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_iterate(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_iterate(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_iterate(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_iterate(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_iterate(_, 'integer':0, _, _, Input, Input).
brachylog_meta_iterate(GlobalVariables, 'default', P, Sub, Input, Output) :-
    I #>= 1,
    brachylog_meta_iterate(GlobalVariables, 'integer':I, P, Sub, Input, Output).
brachylog_meta_iterate(GlobalVariables, 'integer':1, P, Sub, Input, Output) :-
    (   GlobalVariables = 'ignore',
        call(P, Sub, Input, Output)
    ;   dif(GlobalVariables, 'ignore'),
        call(P, GlobalVariables, Sub, Input, Output)
    ).
brachylog_meta_iterate(GlobalVariables, 'integer':I, P, Sub, Input, Output) :-
    I #> 1,
    (   GlobalVariables = 'ignore',
        call(P, Sub, Input, TOutput)
    ;   dif(GlobalVariables, 'ignore'),
        call(P, GlobalVariables, Sub, Input, TOutput)
    ),
    J #= I - 1,
    brachylog_meta_iterate(GlobalVariables, 'integer':J, P, Sub, TOutput, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_LEFTFOLD
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_leftfold(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_leftfold(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_leftfold(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_leftfold(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_leftfold(GlobalVariables, 'default', P, Sub, 'string':S, Output) :-
    brachylog_elements('default', 'string':S, E),
    brachylog_meta_leftfold(GlobalVariables, 'default', P, Sub, E, O),
    (   brachylog_concatenate('default', O, X),
        X = 'string':_ ->
        Output = X
    ;   Output = O
    ).
brachylog_meta_leftfold(GlobalVariables, 'default', P, Sub, 'integer':Input, Output) :-
    brachylog_elements('default', 'integer':Input, E),
    brachylog_meta_leftfold(GlobalVariables, 'default', P, Sub, E, O),
    (   brachylog_concatenate('default', O, X),
        X = 'integer':_ ->
        Output = X
    ;   Output = O
    ).
brachylog_meta_leftfold(_, 'default', _P, _Sub, [], []).
brachylog_meta_leftfold(_, 'default', _P, _Sub, [X], [X]).
brachylog_meta_leftfold(GlobalVariables, 'default', P, Sub, [H,I|T], Output) :-
    brachylog_meta_leftfold_(GlobalVariables, P, Sub, [I|T], H, Output).

brachylog_meta_leftfold_(_, _P, _Sub, [], Output, Output).
brachylog_meta_leftfold_(GlobalVariables, P, Sub, [H|T], A, Output) :-
    (   GlobalVariables = 'ignore',
        call(P, Sub, [A,H], E)
    ;   dif(GlobalVariables, 'ignore'),
        call(P, GlobalVariables, Sub, [A,H], E)
    ),
    brachylog_meta_leftfold_(GlobalVariables, P, Sub, T, E, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_MAP
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_map(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_map(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_map(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_map(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_map(GlobalVariables, 'integer':0, P, Sub, Input, Output) :-
    (   GlobalVariables = 'ignore',
        call(P, Sub, Input, Output)
    ;   dif(GlobalVariables, 'ignore'),
        call(P, GlobalVariables, Sub, Input, Output)
    ).
brachylog_meta_map(GlobalVariables, 'default', P, Sub, Input, Output) :-
    brachylog_meta_map(GlobalVariables, 'integer':1, P, Sub, Input, Output).
brachylog_meta_map(GlobalVariables, 'integer':I, P, Sub, 'string':S, Output) :-
    brachylog_elements('default', 'string':S, E),
    brachylog_meta_map(GlobalVariables, 'integer':I, P, Sub, E, O),
    (   brachylog_concatenate('default', O, X),
        X = 'string':_ ->
        Output = X
    ;   Output = O
    ).
brachylog_meta_map(GlobalVariables, 'integer':I, P, Sub, 'integer':Input, Output) :-
    brachylog_elements('default', 'integer':Input, E),
    brachylog_meta_map(GlobalVariables, 'integer':I, P, Sub, E, O),
    (   brachylog_concatenate('default', O, X),
        X = 'integer':_ ->
        Output = X
    ;   Output = O
    ).
brachylog_meta_map(GlobalVariables, 'integer':1, P, Sub, Input, Output) :-
    (   GlobalVariables = 'ignore',
        Pred =.. [P, Sub]
    ;   dif(GlobalVariables, 'ignore'),
        Pred =.. [P, GlobalVariables, Sub]
    ),
    is_brachylog_list(Input),
    maplist(Pred, Input, Output).
brachylog_meta_map(GlobalVariables, 'integer':I, P, Sub, Input, Output) :-
    I #> 1,
    J #= I - 1,
    is_brachylog_list(Input),
    maplist(brachylog_meta_map(GlobalVariables, 'integer':J, P, Sub), Input, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_NONEXISTENCE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_nonexistence(GlobalVariables, 'first', P, Sub, [I|Input], Arg) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_nonexistence(GlobalVariables, P, Sub, Arg, I).
brachylog_meta_nonexistence(GlobalVariables, 'last', P, Sub, Input, Arg) :-
    reverse(Input, [I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_nonexistence(GlobalVariables, P, Sub, Arg, I).
brachylog_meta_nonexistence(GlobalVariables, 'integer':I, P, Sub, Input, Input) :-
    dif(I, 'default'),
    brachylog_meta_nonexistence(GlobalVariables, P, Sub, Input, 'integer':I).
brachylog_meta_nonexistence(GlobalVariables, 'default', P, Sub, Input, Output) :-
    brachylog_meta_nonexistence(GlobalVariables, P, Sub, Input, Output).

brachylog_meta_nonexistence(GlobalVariables, P, Sub, Input, Output) :-
    brachylog_meta_map(GlobalVariables, 'default', P, Sub, Input, T),
    brachylog_zip('default', [T,[Output]], Z),
    brachylog_meta_map(GlobalVariables, 'default', brachylog_different, 'default', Z, _).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_ORDERBY
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_orderby(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_orderby(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_orderby(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_orderby(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_orderby(GlobalVariables, 'default', P, Sub, Input, Output) :-
    brachylog_meta_orderby(GlobalVariables, 'integer':0, P, Sub, Input, Output).
brachylog_meta_orderby(GlobalVariables, 'integer':0, P, Sub, Input, Output) :-
    (   is_brachylog_list(Input) -> FixedInput = Input
    ;   brachylog_elements('default', Input, FixedInput)
    ),
    brachylog_meta_map(GlobalVariables, 'default', P, Sub, FixedInput, L),
    brachylog_zip('default', [L,Input], L2),
    brachylog_order(integer:0, L2, SL2),
    (   SL2 = [] ->
        Output = []
    ;   brachylog_zip('default', SL2, [_,Output])
    ).
brachylog_meta_orderby(GlobalVariables, 'integer':1, P, Sub, Input, Output) :-
    (   is_brachylog_list(Input) -> FixedInput = Input
    ;   brachylog_elements('default', Input, FixedInput)
    ),
    brachylog_meta_map(GlobalVariables, 'default', P, Sub, FixedInput, L),
    brachylog_zip('default', [L,Input], L2),
    brachylog_order(integer:1, L2, SL2),
    (   SL2 = [] ->
        Output = []
    ;   brachylog_zip('default', SL2, [_,Output])
    ).
brachylog_meta_orderby(GlobalVariables, 'integer':2, P, Sub, Input, Output) :-
    (   is_brachylog_list(Input) -> FixedInput = Input
    ;   brachylog_elements('default', Input, FixedInput)
    ),
    brachylog_meta_map(GlobalVariables, 'default', P, Sub, FixedInput, L),
    brachylog_order(integer:0, L, Output).
brachylog_meta_orderby(GlobalVariables, 'integer':3, P, Sub, Input, Output) :-
    (   is_brachylog_list(Input) -> FixedInput = Input
    ;   brachylog_elements('default', Input, FixedInput)
    ),
    brachylog_meta_map(GlobalVariables, 'default', P, Sub, FixedInput, L),
    brachylog_order(integer:1, L, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_SELECT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_select(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_select(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_select(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_select(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_select(GlobalVariables, 'default', P, Sub, 'string':S, Output) :-
    brachylog_elements('default', 'string':S, E),
    brachylog_meta_select(GlobalVariables, 'default', P, Sub, E, O),
    (   brachylog_concatenate('default', O, X),
        X = 'string':_ ->
        Output = X
    ;   Output = O
    ).
brachylog_meta_select(GlobalVariables, 'default', P, Sub, 'integer':S, Output) :-
    brachylog_elements('default', 'integer':S, E),
    brachylog_meta_select(GlobalVariables, 'default', P, Sub, E, O),
    (   brachylog_concatenate('default', O, X),
        X = 'integer':_ ->
        Output = X
    ;   Output = O
    ).
brachylog_meta_select(_, 'default', _, _, [], []).
brachylog_meta_select(GlobalVariables, 'default', P, Sub, [H|T], Output) :-
    (   (   GlobalVariables = 'ignore',
            call(P, Sub, H, H2)
        ;   dif(GlobalVariables, 'ignore'),
            call(P, GlobalVariables, Sub, H, H2)
        )
    *-> Output = [H2|T2]
    ;   Output = T2
    ),
    brachylog_meta_select(GlobalVariables, 'default', P, Sub, T, T2).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_TAIL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_tail(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_tail(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_tail(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_tail(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_tail(GlobalVariables, 'default', P, Sub, Input, Output) :-
    brachylog_meta_tail(GlobalVariables, 'integer':1, P, Sub, Input, Output).
brachylog_meta_tail(_, 'integer':0, _, _, Input, Input).
brachylog_meta_tail(GlobalVariables, 'integer':I, P, Sub, Input, Output) :-
    I #> 0,
    brachylog_tail('integer':I, Input, Tails),
    brachylog_knife('integer':I, Input, Heads),
    brachylog_meta_map(GlobalVariables, 'default', P, Sub, Tails, NewTails),
    brachylog_concatenate('default', [Heads,NewTails], Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_UNIQUE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_unique(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_unique(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_unique(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_unique(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_unique(GlobalVariables, 'default', P, Sub, Input, Output) :-
    brachylog_meta_unique_(GlobalVariables, 1, -1, P, Sub, Input, [], Output).
brachylog_meta_unique(_, 'integer':0, _, _, _, []).
brachylog_meta_unique(GlobalVariables, 'integer':I, P, Sub, Input, Output) :-
    brachylog_meta_unique_(GlobalVariables, 1, I, P, Sub, Input, [], Output).

brachylog_meta_unique_(_, _, 0, _, _, _, ROutput, Output) :-
    reverse(ROutput, Output).
brachylog_meta_unique_(GlobalVariables, Nth, J, P, Sub, Input, A, Output) :-
    J #\= 0,
    (   call_nth(   (   GlobalVariables = 'ignore',
                        call(P, Sub, Input, X)
                    ;   dif(GlobalVariables, 'ignore'),
                        call(P, GlobalVariables, Sub, Input, X)
                    ), Nth) ->
        (   \+ member(X, A) ->
            M #= Nth + 1,
            K #= J - 1,
            brachylog_meta_unique_(GlobalVariables, M, K, P, Sub, Input, [X|A], Output)
        ;   M #= Nth + 1,
            brachylog_meta_unique_(GlobalVariables, M, J, P, Sub, Input, A, Output)
        )
    ;   reverse(A, Output)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_VERIFY
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_verify(GlobalVariables, 'first', P, Sub, [I|Input], Arg) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_verify(GlobalVariables, P, Sub, Arg, I).
brachylog_meta_verify(GlobalVariables, 'last', P, Sub, Input, Arg) :-
    reverse(Input, [I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_verify(GlobalVariables, P, Sub, Arg, I).
brachylog_meta_verify(GlobalVariables, 'integer':I, P, Sub, Input, Input) :-
    dif(I, 'default'),
    brachylog_meta_verify(GlobalVariables, P, Sub, Input, 'integer':I).
brachylog_meta_verify(GlobalVariables, 'default', P, Sub, Input, Output) :-
    brachylog_meta_verify(GlobalVariables, P, Sub, Input, Output).

brachylog_meta_verify(GlobalVariables, P, Sub, Input, Output) :-
    brachylog_length('default', Input, L),
    brachylog_length('default', T, L),
    brachylog_equal('default', T, _),
    brachylog_head('default', T, Output),
    brachylog_meta_map(GlobalVariables, 'default', P, Sub, Input, T).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_META_ZIP
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_meta_zip(GlobalVariables, 'first', P, Sub, ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_meta_zip(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_zip(GlobalVariables, 'last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_meta_zip(GlobalVariables, 'integer':I, P, Sub, Arg, Output).
brachylog_meta_zip(GlobalVariables, 'default', P, Sub, Arg, Output) :-
    brachylog_meta_map(GlobalVariables, 'default', P, Sub, Arg, O),
    brachylog_zip('default', [Arg,O], Output).
brachylog_meta_zip(GlobalVariables, 'integer':0, P, Sub, Arg, Output) :-
    brachylog_meta_find(GlobalVariables, 'default', P, Sub, Arg, O),
    brachylog_zip('default', [Arg,O], Output).
brachylog_meta_zip(GlobalVariables, 'integer':1, P, Sub, Arg, Output) :-
    brachylog_meta_map(GlobalVariables, 'default', P, Sub, Arg, O),
    brachylog_zip('default', [O,Arg], Output).
brachylog_meta_zip(GlobalVariables, 'integer':2, P, Sub, Arg, Output) :-
    brachylog_meta_find(GlobalVariables, 'default', P, Sub, Arg, O),
    brachylog_zip('default', [O,Arg], Output).
