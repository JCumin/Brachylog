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


:- module(metapredicates, [brachylog_meta_find/5,
                           brachylog_meta_groupby/5,
                           brachylog_meta_iterate/5,
                           brachylog_meta_map/5,
                           brachylog_meta_orderby/5,
                           brachylog_meta_select/5
                          ]).

:- use_module(library(clpfd)).
:- use_module(predicates).
    
    
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
    brachylog_meta_groupby(I, P, Sub, Arg, Output).
brachylog_meta_groupby('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_groupby(I, P, Sub, Arg, Output).
brachylog_meta_groupby('default', P, Sub, Input, Output) :-
    brachylog_meta_map('default', P, Sub, Input, L),
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
    brachylog_meta_iterate(I, P, Sub, Arg, Output).
brachylog_meta_iterate('last', P, Sub, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   T = Arg
    ),
    brachylog_meta_iterate(I, P, Sub, Arg, Output).
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
brachylog_meta_map('integer':1, P, Sub, Input, Output) :-
    Pred =.. [P, Sub],
    maplist(Pred, Input, Output).
brachylog_meta_map('integer':I, P, Sub, Input, Output) :-
    I #> 1,
    J #= I - 1,
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
    brachylog_meta_map('default', P, Sub, Input, L),
    brachylog_zip('default', [L,Input], L2),
    msort(L2, SL2),
    brachylog_zip('default', SL2, [_,Output]).
brachylog_meta_orderby('integer':1, P, Sub, Input, Output) :-
    brachylog_meta_map('default', P, Sub, Input, L),
    brachylog_zip('default', [L,Input], L2),
    msort(L2, RSL2),
    reverse(RSL2, SL2),
    brachylog_zip('default', SL2, [_,Output]).
brachylog_meta_orderby('integer':2, P, Sub, Input, Output) :-
    brachylog_meta_map('default', P, Sub, Input, L),
    msort(L, Output).
brachylog_meta_orderby('integer':3, P, Sub, Input, Output) :-
    brachylog_meta_map('default', P, Sub, Input, L),
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
brachylog_meta_select('default', _, _, [], []).
brachylog_meta_select('default', P, Sub, [H|T], Output) :-
    (   call(P, Sub, H, H2) *->
        Output = [H2|T2]
    ;   Output = T2
    ),
    brachylog_meta_select('default', P, Sub, T, T2).
