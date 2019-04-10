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


:- module(predicates, [%Symbols                                         % Reversed version
                       brachylog_lessequal/3,                           brachylog_lessequal_reversed/3,
                       brachylog_greaterequal/3,                        brachylog_greaterequal_reversed/3,
                       brachylog_contains/3,                            brachylog_contains_reversed/3,
                       brachylog_in/3,                                  brachylog_in_reversed/3,
                       brachylog_superset/3,                            brachylog_superset_reversed/3,
                       brachylog_subset/3,                              brachylog_subset_reversed/3,
                       brachylog_reverse/3,                             brachylog_reverse_reversed/3,
                       brachylog_call_predicate/3,                      brachylog_call_predicate_reversed/3,
                       brachylog_circular_permute_counterclockwise/3,   brachylog_circular_permute_counterclockwise_reversed/3,
                       brachylog_circular_permute_clockwise/3,          brachylog_circular_permute_clockwise_reversed/3,
                       brachylog_root/3,                                brachylog_root_reversed/3,
                       brachylog_ceil/3,                                brachylog_ceil_reversed/3,
                       brachylog_floor/3,                               brachylog_floor_reversed/3,
                       brachylog_range_ascending/3,                     brachylog_range_ascending_reversed/3,
                       brachylog_range_descending/3,                    brachylog_range_descending_reversed/3,
                       brachylog_natural_integer/3,                     brachylog_natural_integer_reversed/3,
                       brachylog_integer/3,                             brachylog_integer_reversed/3,
                       brachylog_float/3,                               brachylog_float_reversed/3,
                       brachylog_different/3,                           brachylog_different_reversed/3,
                       brachylog_identity/3,                            brachylog_identity_reversed/3,
                       brachylog_integer_division/3,                    brachylog_integer_division_reversed/3,
                       brachylog_multiply/3,                            brachylog_multiply_reversed/3,
                       brachylog_modulo/3,                              brachylog_modulo_reversed/3,
                       brachylog_exp/3,                                 brachylog_exp_reversed/3,
                       brachylog_plus/3,                                brachylog_plus_reversed/3,
                       brachylog_minus/3,                               brachylog_minus_reversed/3,
                       brachylog_divide/3,                              brachylog_divide_reversed/3,
                       brachylog_less/3,                                brachylog_less_reversed/3,
                       brachylog_equal/3,                               brachylog_equal_reversed/3,
                       brachylog_greater/3,                             brachylog_greater_reversed/3,
                       brachylog_transpose/3,                           brachylog_transpose_reversed/3,
                       brachylog_power/3,                               brachylog_power_reversed/3,

                       %Lowercase letters                               % Reversed version
                       brachylog_adfix/3,                               brachylog_adfix_reversed/3,
                       brachylog_behead/3,                              brachylog_behead_reversed/3,
                       brachylog_concatenate/3,                         brachylog_concatenate_reversed/3,
                       brachylog_duplicates/3,                          brachylog_duplicates_reversed/3,
                       brachylog_factors/3,                             brachylog_factors_reversed/3,
                       brachylog_group/3,                               brachylog_group_reversed/3,
                       brachylog_head/3,                                brachylog_head_reversed/3,
                       brachylog_index/3,                               brachylog_index_reversed/3,
                       brachylog_juxtapose/3,                           brachylog_juxtapose_reversed/3,
                       brachylog_knife/3,                               brachylog_knife_reversed/3,
                       brachylog_length/3,                              brachylog_length_reversed/3,
                       brachylog_order/3,                               brachylog_order_reversed/3,
                       brachylog_permute/3,                             brachylog_permute_reversed/3,
                       brachylog_substring/3,                           brachylog_substring_reversed/3,
                       brachylog_tail/3,                                brachylog_tail_reversed/3,
                       brachylog_write/3,                               brachylog_write_reversed/3,
                       brachylog_xterminate/3,                          brachylog_xterminate_reversed/3,
                       brachylog_zip/3,                                 brachylog_zip_reversed/3,

                       %Lowercase letters with dot below                % Reversed version
                       brachylog_to_codes/3,                            brachylog_to_codes_reversed/3,
                       brachylog_blocks/3,                              brachylog_blocks_reversed/3,
                       brachylog_dichotomize/3,                         brachylog_dichotomize_reversed/3,
                       brachylog_elements/3,                            brachylog_elements_reversed/3,
                       brachylog_to_number/3,                           brachylog_to_number_reversed/3,
                       brachylog_lowercase/3,                           brachylog_lowercase_reversed/3,
                       brachylog_split_lines/3,                         brachylog_split_lines_reversed/3,
                       brachylog_occurences/3,                          brachylog_occurences_reversed/3,
                       brachylog_random_element/3,                      brachylog_random_element_reversed/3,
                       brachylog_shuffle/3,                             brachylog_shuffle_reversed/3,
                       brachylog_uppercase/3,                           brachylog_uppercase_reversed/3,
                       brachylog_writeln/3,                             brachylog_writeln_reversed/3,

                       %Lowercase letters with dot above                % Reversed version
                       brachylog_absolute_value/3,                      brachylog_absolute_value_reversed/3,
                       brachylog_base/3,                                brachylog_base_reversed/3,
                       brachylog_coerce/3,                              brachylog_coerce_reversed/3,
                       brachylog_prime_decomposition/3,                 brachylog_prime_decomposition_reversed/3,
                       brachylog_factorial/3,                           brachylog_factorial_reversed/3,
                       brachylog_groups/3,                              brachylog_groups_reversed/3,
                       brachylog_matrix/3,                              brachylog_matrix_reversed/3,
                       brachylog_negate/3,                              brachylog_negate_reversed/3,
                       brachylog_prime/3,                               brachylog_prime_reversed/3,
                       brachylog_random_number/3,                       brachylog_random_number_reversed/3,
                       brachylog_sign/3,                                brachylog_sign_reversed/3,
                       brachylog_to_string/3,                           brachylog_to_string_reversed/3,

                       %Label                                           % Reversed version
                       brachylog_label/3,                               brachylog_label_reversed/3
                      ]).
                       
:- use_module(library(clpfd)).
:- use_module(utils).

:- multifile clpfd:run_propagator/2.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_LESSEQUAL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_lessequal_reversed(S, I, O) :-
    brachylog_lessequal(S, O, I).
brachylog_lessequal('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_lessequal('integer':I, Arg, Output).
brachylog_lessequal('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_lessequal('integer':I, Arg, Output).
brachylog_lessequal('default', Input, Output) :-
    brachylog_lessequal('integer':0, Input, Output).
brachylog_lessequal('integer':0, 'integer':I1, 'integer':I2) :-
    I1 #=< I2.
brachylog_lessequal('integer':0, 'float':I1, 'integer':I2) :-
    nonvar(I1),
    brachylog_label('default', 'integer':I2, _),
    I1 =< I2.
brachylog_lessequal('integer':0, 'integer':I1, 'float':I2) :-
    nonvar(I2),
    brachylog_label('default', 'integer':I1, _),
    I1 =< I2.
brachylog_lessequal('integer':0, 'float':I1, 'float':I2) :-
    nonvar(I1),
    nonvar(I2),
    I1 =< I2.
brachylog_lessequal('integer':1, [], []).
brachylog_lessequal('integer':1, ['integer':I], ['integer':I]).
brachylog_lessequal('integer':1, ['integer':I,'integer':J|T], ['integer':I,'integer':J|T]) :-
    I #=< J,
    brachylog_lessequal('integer':1, ['integer':J|T], ['integer':J|T]).
     
     
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_GREATEREQUAL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_greaterequal_reversed(S, I, O) :-
    brachylog_greaterequal(S, O, I).
brachylog_greaterequal('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_greaterequal('integer':I, Arg, Output).
brachylog_greaterequal('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_greaterequal('integer':I, Arg, Output).
brachylog_greaterequal('default', Input, Output) :-
    brachylog_greaterequal('integer':0, Input, Output).
brachylog_greaterequal('integer':0, 'integer':I1, 'integer':I2) :-
    I1 #>= I2.
brachylog_greaterequal('integer':0, 'float':I1, 'integer':I2) :-
    nonvar(I1),
    brachylog_label('default', 'integer':I2, _),
    I1 >= I2.
brachylog_greaterequal('integer':0, 'integer':I1, 'float':I2) :-
    nonvar(I2),
    brachylog_label('default', 'integer':I1, _),
    I1 >= I2.
brachylog_greaterequal('integer':0, 'float':I1, 'float':I2) :-
    nonvar(I1),
    nonvar(I2),
    I1 >= I2.
brachylog_greaterequal('integer':1, [], []).
brachylog_greaterequal('integer':1, ['integer':I], ['integer':I]).
brachylog_greaterequal('integer':1, ['integer':I,'integer':J|T], ['integer':I,'integer':J|T]) :-
    I #>= J,
    brachylog_greaterequal('integer':1, ['integer':J|T], ['integer':J|T]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_CONTAINS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_contains_reversed(S, I, O) :-
    brachylog_contains(S, O, I).
brachylog_contains(Sub, Input, Output) :-
    brachylog_in(Sub, Output, Input).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_IN
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_in_reversed(S, I, O) :-
    brachylog_in(S, O, I).
brachylog_in('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_in('integer':I, Arg, Output).
brachylog_in('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_in('integer':I, Arg, Output).
brachylog_in('default', 'string':L, 'string':[M]) :-
    nth0(_, L, M).
brachylog_in('integer':S, 'string':L, 'string':[M]) :-
    nth0(S, L, M).
brachylog_in('default', 'integer':0, 'integer':0).
brachylog_in('integer':0, 'integer':0, 'integer':0).
brachylog_in('default', 'integer':I, 'integer':J) :-
    H #\= 0,
    integer_value('integer':_:[H|T], I),
    nth0(_, [H|T], M),
    integer_value('integer':'positive':[M], J).
brachylog_in('integer':S, 'integer':I, 'integer':J) :-
    H #\= 0,
    integer_value('integer':_:[H|T], I),
    nth0(S, [H|T], M),
    integer_value('integer':'positive':[M], J).
brachylog_in('default', L, M) :-
    is_brachylog_list(L),
    nth0(_, L, M).
brachylog_in('integer':S, L, M) :-
    is_brachylog_list(L),
    nth0(S, L, M).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_SUPERSET
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_superset_reversed(S, I, O) :-
    brachylog_superset(S, O, I).
brachylog_superset(Sub, Input, Output) :-
    brachylog_subset(Sub, Output, Input).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_SUBSET
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_subset_reversed(S, I, O) :-
    brachylog_subset(S, O, I).
brachylog_subset('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_subset('integer':I, Arg, Output).
brachylog_subset('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_subset('integer':I, Arg, Output).
brachylog_subset('default', Input, Output) :-
    brachylog_subset('integer':0, Input, Output).
brachylog_subset('integer':0, 'string':S, 'string':T) :-
    brachylog_subset_recur(S, T).
brachylog_subset('integer':0, 'integer':0, 'integer':0).
brachylog_subset('integer':0, 'integer':I, 'integer':J) :-
    H #\= 0,
    dif(M, []),
    integer_value('integer':Sign:[H|L], I),
    brachylog_subset_recur([H|L], M),
    integer_value('integer':Sign:M, J).
brachylog_subset('integer':0, 'float':F, 'float':G) :-
    Sign is abs(F)/F,
    AF is abs(F),
    number_chars(AF, C),
    brachylog_subset_recur(C, D),
    dif(D, []),
    \+ (D = ['.'|_] ; reverse(D, ['.'|_])),
    number_chars(AG,D),
    G is Sign*AG.
brachylog_subset('integer':0, L, S) :-
    is_brachylog_list(L),
    brachylog_subset_recur(L, S).

brachylog_subset_recur(L, S) :-
    var(S),
    length(L, Length),
    between(0, Length, I),
    J #= Length - I,
    length(S, J),
    brachylog_subset_recur_(L, S).
brachylog_subset_recur(L, S) :-
    nonvar(S),
    length(S, Length),
    I #>= Length,
    length(L, I),
    brachylog_subset_recur_(L, S).

brachylog_subset_recur_([], []).
brachylog_subset_recur_([H|T], [H|T2]) :-
    brachylog_subset_recur_(T, T2).
brachylog_subset_recur_([_|T], T2) :-
    brachylog_subset_recur_(T, T2).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_REVERSE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_reverse_reversed(S, I, O) :-
    brachylog_reverse(S, O, I).
brachylog_reverse('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_reverse('integer':I, Arg, Output).
brachylog_reverse('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_reverse('integer':I, Arg, Output).
brachylog_reverse('default', Input, Output) :-
    brachylog_reverse('integer':0, Input, Output).
brachylog_reverse('integer':0, 'string':S, 'string':R) :-
    reverse(S, R).
brachylog_reverse('integer':0, 'integer':I, 'integer':R) :-
    nonvar(I),
    H #\= 0,
    A #\= 0,
    integer_value('integer':Sign:[H|T], I),
    reverse([H|T], B),
    append(Zeroes, [A|Rest], B),
    maplist(=(0), Zeroes),
    integer_value('integer':Sign:[A|Rest], R).
brachylog_reverse('integer':0, 'integer':0, 'integer':0).
brachylog_reverse('integer':0, 'integer':I, 'integer':R) :-
    var(I),
    H #\= 0,
    A #\= 0,
    integer_value('integer':Sign:[A|B], R),
    reverse(L, [A|B]),
    append(Zeroes, [H|T], L),
    maplist(=(0), Zeroes),
    integer_value('integer':Sign:[H|T], I).
brachylog_reverse('integer':0, List, R) :-
    reverse(List, R).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_CALL_PREDICATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_call_predicate_reversed(S, I, O) :-
    brachylog_call_predicate(S, O, I).
brachylog_call_predicate('first'-GlobalVariables, ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_call_predicate(GlobalVariables, 'integer':I, Arg, Output).
brachylog_call_predicate('last'-GlobalVariables, Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_call_predicate(GlobalVariables, 'integer':I, Arg, Output).
brachylog_call_predicate(Name-GlobalVariables, Arg, Output) :-
    brachylog_call_predicate(GlobalVariables, Name, Arg, Output).
brachylog_call_predicate(GlobalVariables, CallingPredName, Input, Output) :-
    atom(CallingPredName),
    call(CallingPredName, GlobalVariables, 'integer':0, Input, Output).
brachylog_call_predicate(GlobalVariables, 'integer':I, Input, Output) :-
    (   I = 0,
        PredName = 'brachylog_main'
    ;   I #> 0,
        atomic_list_concat(['brachylog_predicate_',I], PredName)
    ),
    call(PredName, GlobalVariables, 'integer':0, Input, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_CIRCULAR_PERMUTE_COUNTERCLOCKWISE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_circular_permute_counterclockwise_reversed(S, I, O) :-
    brachylog_circular_permute_counterclockwise(S, O, I).
brachylog_circular_permute_counterclockwise('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_circular_permute_counterclockwise('integer':I, Arg, Output).
brachylog_circular_permute_counterclockwise('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_circular_permute_counterclockwise('integer':I, Arg, Output).
brachylog_circular_permute_counterclockwise('default', Input, Output) :-
    brachylog_circular_permute_counterclockwise('integer':1, Input, Output).
brachylog_circular_permute_counterclockwise('integer':0, Input, Input).
brachylog_circular_permute_counterclockwise('integer':1, 'string':[], 'string':[]).
brachylog_circular_permute_counterclockwise('integer':1, 'string':[H|T], 'string':S) :-
    append(T, [H], S).
brachylog_circular_permute_counterclockwise('integer':1, [], []).
brachylog_circular_permute_counterclockwise('integer':1, [H|T], S) :-
    append(T, [H], S).
brachylog_circular_permute_counterclockwise('integer':1, 'integer':0, 'integer':0).
brachylog_circular_permute_counterclockwise('integer':1, 'integer':I, 'integer':J) :-
    dif(H, 0),
    integer_value('integer':Sign:[H|T], I),
    append(T, [H], S),
    integer_value('integer':Sign:S, J).
brachylog_circular_permute_counterclockwise('integer':I, Input, Output) :-
    I #> 1,
    brachylog_meta_iterate(ignore, 'integer':I, brachylog_circular_permute_counterclockwise, 'default', Input, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_CIRCULAR_PERMUTE_CLOCKWISE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_circular_permute_clockwise_reversed(S, I, O) :-
    brachylog_circular_permute_clockwise(S, O, I).
brachylog_circular_permute_clockwise('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_circular_permute_clockwise('integer':I, Arg, Output).
brachylog_circular_permute_clockwise('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_circular_permute_clockwise('integer':I, Arg, Output).
brachylog_circular_permute_clockwise('default', Input, Output) :-
    brachylog_circular_permute_clockwise('integer':1, Input, Output).
brachylog_circular_permute_clockwise('integer':0, Input, Input).
brachylog_circular_permute_clockwise('integer':1, 'string':[], 'string':[]).
brachylog_circular_permute_clockwise('integer':1, 'string':L, 'string':S) :-
    append(T, [H], L),
    S = [H|T].
brachylog_circular_permute_clockwise('integer':1, [], []).
brachylog_circular_permute_clockwise('integer':1, [A|B], S) :-
    append(T, [H], [A|B]),
    S = [H|T].
brachylog_circular_permute_clockwise('integer':1, 'integer':0, 'integer':0).
brachylog_circular_permute_clockwise('integer':1, 'integer':I, 'integer':J) :-
    dif(H2, 0),
    integer_value('integer':Sign:[H2|T2], I),
    append(T, [H], [H2|T2]),
    S = [H|T],
    integer_value('integer':Sign:S, J).
brachylog_circular_permute_clockwise('integer':I, Input, Output) :-
    I #> 1,
    brachylog_meta_iterate(ignore, 'integer':I, brachylog_circular_permute_clockwise, 'default', Input, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_ROOT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_root_reversed(S, I, O) :-
    brachylog_root(S, O, I).
brachylog_root('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_root('integer':I, Arg, Output).
brachylog_root('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_root('integer':I, Arg, Output).
brachylog_root('default', Input, Output) :-
    brachylog_root('integer':2, Input, Output).
brachylog_root('integer':I,'integer':E, Type:R) :-
    (   E #= R^I ->
        Type = 'integer'
    ;   brachylog_label('default', ['integer':I, 'integer':E], _),
        R is E^(1/I),
        Type = 'float'
    ).
brachylog_root('integer':I,'float':E, 'float':R) :-
    nonvar(E),
    brachylog_label('default', 'integer':I, _),
    R is E^(1/I).
brachylog_root('float':I,'integer':E, 'float':R) :-
    brachylog_label('default', 'integer':E, _),
    R is E^(1/I).
brachylog_root('float':I,'float':E, 'float':R) :-
    nonvar(E),
    R is E^(1/I).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_CEIL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_ceil_reversed(S, I, O) :-
    brachylog_ceil(S, O, I).
brachylog_ceil('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_ceil('integer':I, Arg, Output).
brachylog_ceil('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_ceil('integer':I, Arg, Output).
brachylog_ceil('default', Input, Output) :-
    brachylog_ceil('integer':0, Input, Output).
brachylog_ceil('integer':0, [H|T], Output) :-
    foldl(scompare(@>), [H|T], H, Output).
brachylog_ceil('integer':1, 'integer':I, 'integer':I).
brachylog_ceil('integer':1, 'float':I, 'integer':J) :-
    nonvar(I),
    J is ceil(I).
 
 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_FLOOR
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_floor_reversed(S, I, O) :-
    brachylog_floor(S, O, I).
brachylog_floor('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_floor('integer':I, Arg, Output).
brachylog_floor('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_floor('integer':I, Arg, Output).
brachylog_floor('default', Input, Output) :-
    brachylog_floor('integer':0, Input, Output).
brachylog_floor('integer':0, [H|T], Output) :-
    foldl(scompare(@<), [H|T], H, Output).
brachylog_floor('integer':1, 'integer':I, 'integer':I).
brachylog_floor('integer':1, 'float':I, 'integer':J) :-
    nonvar(I),
    J is floor(I).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_RANGE_ASCENDING
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_range_ascending_reversed(S, I, O) :-
    brachylog_range_ascending(S, O, I).
brachylog_range_ascending('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_range_ascending('integer':I, Arg, Output).
brachylog_range_ascending('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_range_ascending('integer':I, Arg, Output).
brachylog_range_ascending('default', Input, Output) :-
    brachylog_range_ascending('integer':0, Input, Output).
brachylog_range_ascending('integer':0, 'integer':Input, Output) :-
    (   0 #=< Input,
        brachylog_range_ascending_(0, Input, Output)
    ;   0 #> Input,
        brachylog_range_ascending_(Input, 0, Output)
    ).
brachylog_range_ascending('integer':1, 'integer':Input, Output) :-
    (   1 #=< Input,
        brachylog_range_ascending_(1, Input, Output)
    ;   1 #> Input,
        brachylog_range_ascending_(Input, 1, Output)
    ).
brachylog_range_ascending('integer':2, ['integer':X,'integer':Y], Output) :-
    (   X #=< Y,
        brachylog_range_ascending_(X, Y, Output)
    ;   X #> Y,
        brachylog_range_ascending_(Y, X, Output)
    ).
brachylog_range_ascending('integer':3, ['integer':X,'integer':Y], Output) :-
    (   X #=< Y,
        Y2 #= Y - 1,
        brachylog_range_ascending_(X, Y2, Output)
    ;   X #> Y,
        X2 #= X - 1,
        brachylog_range_ascending_(Y, X2, Output)
    ).
brachylog_range_ascending('integer':4, ['integer':X,'integer':Y], Output) :-
    (   X #=< Y,
        X2 #= X + 1,
        brachylog_range_ascending_(X2, Y, Output)
    ;   X #> Y,
        Y2 #= Y + 1,
        brachylog_range_ascending_(Y2, X, Output)
    ).
brachylog_range_ascending('integer':5, 'integer':Input, Output) :-
    (   0 #=< Input,
        I2 #= Input - 1,
        brachylog_range_ascending_(0, I2, Output)
    ;   0 #> Input,
        I2 #= Input + 1,
        brachylog_range_ascending_(I2, 0, Output)
    ).
brachylog_range_ascending('integer':6, 'integer':Input, Output) :-
    (   1 #=< Input,
        I2 #= Input - 1,
        brachylog_range_ascending_(1, I2, Output)
    ;   1 #> Input,
        I2 #= Input + 1,
        brachylog_range_ascending_(I2, 1, Output)
    ).

brachylog_range_ascending_(I, S, ['integer':I|R]) :-
    I #=< S,
    if_(I = S,
        R = [],
        (   J #= I + 1,
            predicates:brachylog_range_ascending_(J, S, R)
        )
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_RANGE_DESCENDING
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_range_descending_reversed(S, I, O) :-
    brachylog_range_descending(S, O, I).
brachylog_range_descending('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_range_descending('integer':I, Arg, Output).
brachylog_range_descending('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_range_descending('integer':I, Arg, Output).
brachylog_range_descending('default', Input, Output) :-
    brachylog_range_descending('integer':0, Input, Output).
brachylog_range_descending('integer':Sub, Input, Output) :-
    brachylog_range_ascending('integer':Sub, Input, ROutput),
    brachylog_reverse('default', ROutput, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_NATURAL_INTEGER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_natural_integer_reversed(S, I, O) :-
    brachylog_natural_integer(S, O, I).
brachylog_natural_integer('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_natural_integer('integer':I, Arg, Output).
brachylog_natural_integer('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_natural_integer('integer':I, Arg, Output).
brachylog_natural_integer('default', Input, Output) :-
    brachylog_natural_integer('integer':0, Input, Output).
brachylog_natural_integer('integer':I, 'integer':Input, 'integer':Input) :-
    I #>= 0,
    Input #>= I.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_INTEGER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_integer_reversed(S, I, O) :-
    brachylog_integer(S, O, I).
brachylog_integer('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_integer('integer':I, Arg, Output).
brachylog_integer('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_integer('integer':I, Arg, Output).
brachylog_integer('default', 'integer':Input, 'integer':Input) :-
    Input in inf..sup.
brachylog_integer('integer':I, 'integer':Input, 'integer':Input) :-
    I #>= 0,
    Input #=< -I.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_FLOAT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_float_reversed(S, I, O) :-
    brachylog_float(S, O, I).
brachylog_float('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_float('integer':I, Arg, Output).
brachylog_float('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_float('integer':I, Arg, Output).
brachylog_float('default', 'float':Input, 'float':Input).
brachylog_float('integer':_, 'integer':Input, 'float':Input).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_DIFFERENT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_different_reversed(S, I, O) :-
    brachylog_different(S, O, I).
brachylog_different('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_different('integer':I, Arg, Output).
brachylog_different('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_different('integer':I, Arg, Output).
brachylog_different('default', 'string':S, 'string':S) :-
    brachylog_different_(S).
brachylog_different('default', [], []).
brachylog_different('default', [H|T], [H|T]) :-
    (   maplist(prepend_integer, L, [H|T]),
        all_different(L)                        % More efficient on integers
    ;   maplist(prepend_string, _, [H|T]),
        brachylog_different_([H|T])
    ;   maplist(is_brachylog_list, [H|T]),
        brachylog_different_([H|T])
    ).
brachylog_different('default', 'integer':I, 'integer':I) :-
    (   integer_value('integer':_:[_], I) ->
        true
    ;   H #\= 0,
        integer_value('integer':_:[H,H2|T], I),
        all_different([H,H2|T])
    ).

brachylog_different_([]).
brachylog_different_([H|T]) :-
    maplist(dif(H), T),
    brachylog_different_(T).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_IDENTITY
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_identity_reversed(S, I, O) :-
    brachylog_identity(S, O, I).
brachylog_identity(_, Input, Input).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_INTEGER_DIVISION
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_integer_division_reversed(S, I, O) :-
    brachylog_integer_division(S, O, I).
brachylog_integer_division('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_integer_division('integer':I, Arg, Output).
brachylog_integer_division('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_integer_division('integer':I, Arg, Output).
brachylog_integer_division('default', Input, Output) :-
    brachylog_integer_division('integer':0, Input, Output).
brachylog_integer_division('integer':0, [], 'integer':1).
brachylog_integer_division('integer':0, ['integer':I1,'integer':I2], 'integer':Division) :-
    brachylog_label('default', ['integer':I1,'integer':I2], _),
    Division #= I1 // I2.
brachylog_integer_division('integer':0, ['float':I1,'integer':I2], 'integer':Division) :-
    brachylog_label('default', 'integer':I2, _),
    nonvar(I1),
    D is I1 / I2,
    brachylog_floor('integer':1, 'float':D, 'integer':Division).
brachylog_integer_division('integer':0, ['integer':I1,'float':I2], 'integer':Division) :-
    brachylog_label('default', 'integer':I1, _),
    nonvar(I2),
    D is I1 / I2,
    brachylog_floor('integer':1, 'float':D, 'integer':Division).
brachylog_integer_division('integer':0, ['float':I1,'float':I2], 'integer':Division) :-
    nonvar(I1),
    nonvar(I2),
    D is I1 / I2,
    brachylog_floor('integer':1, 'float':D, 'integer':Division).
brachylog_integer_division('integer':I, Input, Output) :-
    I #> 0,
    brachylog_integer_division('integer':0, [Input,'integer':I], Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MULTIPLY
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_multiply_reversed(S, I, O) :-
    brachylog_multiply(S, O, I).
brachylog_multiply('first', ['integer':A,'integer':B], Output) :- 
    brachylog_multiply('integer':A, 'integer':B, Output).
brachylog_multiply('last', ['integer':A,'integer':B], Output) :-
    brachylog_multiply('integer':B, 'integer':A, Output).
brachylog_multiply('integer':S, 'integer':I, 'integer':J) :-
    J #= S*I.
brachylog_multiply('integer':S, 'float':F, 'float':G) :-
    nonvar(F),
    G is S*F.
brachylog_multiply('default', [], 'integer':1).
brachylog_multiply('default', [TypeI:I|T], TypeS:Product) :-
    (   TypeI = 'integer',
        TypeF = 'integer',
        (   var(I) ->
            I #> 1,
            F #> 0
        ;   true
        ),
        Product #= I * F,
        TypeS = 'integer',
        brachylog_multiply('default', T, TypeF:F)
    ;   TypeS = 'float',
        brachylog_multiply('default', T, TypeF:F),
        (   TypeF = 'float',
            TypeI = 'integer',
            brachylog_label('default', 'integer':I, _)
        ;   TypeI = 'float',
            nonvar(I),
            TypeF = 'integer',
            brachylog_label('default', 'integer':F, _)
        ;   TypeF = 'float',
            TypeI = 'float',
            nonvar(I)
        ),
        Product is I * F
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MODULO
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_modulo_reversed(S, I, O) :-
    brachylog_modulo(S, O, I).
brachylog_modulo('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_modulo('integer':I, Arg, Output).
brachylog_modulo('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_modulo('integer':I, Arg, Output).
brachylog_modulo('default', Input, Output) :-
    brachylog_modulo('integer':0, Input, Output).
brachylog_modulo('integer':0, ['integer':I1,'integer':I2], 'integer':Rem) :-
    Rem #= I1 mod I2.
brachylog_modulo('integer':I, 'integer':I1, 'integer':Rem) :-
    I #> 0,
    Rem #= I1 mod I.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_EXP
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_exp_reversed(S, I, O) :-
    brachylog_exp(S, O, I).
brachylog_exp('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_exp('integer':I, Arg, Output).
brachylog_exp('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_exp('integer':I, Arg, Output).
brachylog_exp('default', Input, Output) :-
    brachylog_exp('integer':0, Input, Output).
brachylog_exp('integer':0, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is exp(I).
brachylog_exp('integer':0, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is exp(F).
brachylog_exp('integer':1, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is log(I).
brachylog_exp('integer':1, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is log(F).
brachylog_exp('integer':2, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is cos(I).
brachylog_exp('integer':2, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is cos(F).
brachylog_exp('integer':3, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is sin(I).
brachylog_exp('integer':3, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is sin(F).
brachylog_exp('integer':4, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is tan(I).
brachylog_exp('integer':4, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is tan(F).
brachylog_exp('integer':5, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is acos(I).
brachylog_exp('integer':5, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is acos(F).
brachylog_exp('integer':6, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is asin(I).
brachylog_exp('integer':6, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is asin(F).
brachylog_exp('integer':7, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is atan(I).
brachylog_exp('integer':7, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is atan(F).
brachylog_exp('integer':8, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is cosh(I).
brachylog_exp('integer':8, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is cosh(F).
brachylog_exp('integer':9, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is sinh(I).
brachylog_exp('integer':9, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is sinh(F).
brachylog_exp('integer':10, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is tanh(I).
brachylog_exp('integer':10, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is tanh(F).
brachylog_exp('integer':11, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is acosh(I).
brachylog_exp('integer':11, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is acosh(F).
brachylog_exp('integer':12, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is asinh(I).
brachylog_exp('integer':12, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is asinh(F).
brachylog_exp('integer':13, 'integer':I, 'float':Exp) :-
    brachylog_label('default', 'integer':I, _),
    Exp is atanh(I).
brachylog_exp('integer':13, 'float':F, 'float':Exp) :-
    nonvar(F),
    Exp is atanh(F).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_PLUS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_plus_reversed(S, I, O) :-
    brachylog_plus(S, O, I).
brachylog_plus('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_plus('integer':I, Arg, Output).
brachylog_plus('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_plus('integer':I, Arg, Output).
brachylog_plus('default', Input, Output) :-
    brachylog_plus('integer':0, Input, Output).
brachylog_plus('integer':I, 'integer':Input, 'integer':Output) :-
    I #> 0,
    Output #= Input + I.
brachylog_plus('integer':I, 'float':Input, 'float':Output) :-
    I #> 0,
    nonvar(Input),
    Output is Input + I.
brachylog_plus('integer':0, [], 'integer':0).
brachylog_plus('integer':0, [TypeI:I|T], TypeS:Sum) :-
    brachylog_plus('integer':0, T, TypeF:F),
    (   TypeI = 'integer',
        TypeF = 'integer',
        Sum #= I + F,
        TypeS = 'integer'
    ;   TypeS = 'float',
        (   TypeF = 'float',
            TypeI = 'integer',
            brachylog_label('default', 'integer':I, _)
        ;   TypeI = 'float',
            nonvar(I),
            TypeF = 'integer',
            brachylog_label('default', 'integer':F, _)
        ;   TypeF = 'float',
            TypeI = 'float',
            nonvar(I)
        ),
        Sum is I + F
    ).
 
 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MINUS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_minus_reversed(S, I, O) :-
    brachylog_minus(S, O, I).
brachylog_minus('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_minus('integer':I, Arg, Output).
brachylog_minus('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_minus('integer':I, Arg, Output).
brachylog_minus('default', Input, Output) :-
    brachylog_minus('integer':0, Input, Output).
brachylog_minus('integer':I, 'integer':Input, 'integer':Output) :-
    I #> 0,
    Output #= Input - I.
brachylog_minus('integer':I, 'float':Input, 'float':Output) :-
    I #> 0,
    nonvar(Input),
    Output is Input - I.
brachylog_minus('integer':0, [], 'integer':0).
brachylog_minus('integer':0, [TypeI:I|T], TypeS:Sum) :-
    brachylog_minus('integer':0, T, TypeF:F),
    (   TypeI = 'integer',
        TypeF = 'integer',
        Sum #= I - F,
        TypeS = 'integer'
    ;   TypeS = 'float',
        (   TypeF = 'float',
            TypeI = 'integer',
            brachylog_label('default', 'integer':I, _)
        ;   TypeI = 'float',
            nonvar(I),
            TypeF = 'integer',
            brachylog_label('default', 'integer':F, _)
        ;   TypeF = 'float',
            TypeI = 'float',
            nonvar(I)
        ),
        Sum is I - F
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_DIVIDE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_divide_reversed(S, I, O) :-
    brachylog_divide(S, O, I).
brachylog_divide('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_divide('integer':I, Arg, Output).
brachylog_divide('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_divide('integer':I, Arg, Output).
brachylog_divide('default', [], 'integer':1).
brachylog_divide('default', ['integer':I1,'integer':I2], Type:Division) :-
    brachylog_label('default', ['integer':I1,'integer':I2], _),
    I2 #\= 0,
    Division is I1 / I2,
    (   integer(Division) ->
        Type = 'integer'
    ;   Type = 'float'
    ).
brachylog_divide('default', ['integer':0, 'integer':0], 'integer':_).
brachylog_divide('default', ['float':I1,'integer':I2], 'float':Division) :-
    brachylog_label('default', 'integer':I2, _),
    I2 #\= 0,
    nonvar(I1),
    Division is I1 / I2.
brachylog_divide('default', ['float':0.0, 'integer':0], 'float':_).
brachylog_divide('default', ['integer':I1,'float':I2], 'float':Division) :-
    brachylog_label('default', 'integer':I1, _),
    nonvar(I2),
    dif(I2, 0.0),
    Division is I1 / I2.
brachylog_divide('default', ['integer':0, 'float':0.0], 'float':_).
brachylog_divide('default', ['float':I1,'float':I2], 'float':Division) :-
    nonvar(I1),
    nonvar(I2),
    dif(I2, 0.0),
    Division is I1 / I2.
brachylog_divide('default', ['float':0.0, 'float':0.0], 'float':_).
brachylog_divide('integer':1, 'integer':I, 'float':J) :-
    brachylog_label('default', 'integer':I, _),
    J is 1/I.
brachylog_divide('integer':1, 'float':I, 'float':J) :-
    J is 1/I.
brachylog_divide('integer':I, Input, Output) :-
    I #> 1,
    brachylog_divide('integer':0, [Input,'integer':I], Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_LESS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_less_reversed(S, I, O) :-
    brachylog_less(S, O, I).
brachylog_less('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_less('integer':I, Arg, Output).
brachylog_less('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_less('integer':I, Arg, Output).
brachylog_less('default', Input, Output) :-
    brachylog_less('integer':0, Input, Output).
brachylog_less('integer':0, 'integer':I1, 'integer':I2) :-
    I1 #< I2.
brachylog_less('integer':0, 'float':I1, 'integer':I2) :-
    nonvar(I1),
    brachylog_label('default', 'integer':I2, _),
    I1 < I2.
brachylog_less('integer':0, 'integer':I1, 'float':I2) :-
    nonvar(I2),
    brachylog_label('default', 'integer':I1, _),
    I1 < I2.
brachylog_less('integer':0, 'float':I1, 'float':I2) :-
    nonvar(I1),
    nonvar(I2),
    I1 < I2.
brachylog_less('integer':1, [], []).
brachylog_less('integer':1, ['integer':I], ['integer':I]).
brachylog_less('integer':1, ['integer':I,'integer':J|T], ['integer':I,'integer':J|T]) :-
    I #< J,
    brachylog_less('integer':1, ['integer':J|T], ['integer':J|T]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_EQUAL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_equal_reversed(S, I, O) :-
    brachylog_equal(S, O, I).
brachylog_equal('first', [I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_equal(I, Arg, Output).
brachylog_equal('last', Input, Output) :-
    reverse(Input, [I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_equal(I, Arg, Output).
brachylog_equal('default', [], []).
brachylog_equal('default', [H|T], [H|T]) :-
    maplist(=(H), T).
brachylog_equal('default', 'string':L, 'string':L) :-
    brachylog_equal('integer':0, L, L).
brachylog_equal('default', 'integer':0, 'integer':0).
brachylog_equal('default', 'integer':I, 'integer':I) :-
    H #\= 0,
    integer_value('integer':_:[H|T], I),
    brachylog_equal('default', [H|T], [H|T]).
brachylog_equal(Type:I, [Type:I|T], [Type:I|T]) :-
    brachylog_equal('default', [Type:I|T], [Type:I|T]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_GREATER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_greater_reversed(S, I, O) :-
    brachylog_greater(S, O, I).
brachylog_greater('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_greater('integer':I, Arg, Output).
brachylog_greater('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_greater('integer':I, Arg, Output).
brachylog_greater('default', Input, Output) :-
    brachylog_greater('integer':0, Input, Output).
brachylog_greater('integer':0, 'integer':I1, 'integer':I2) :-
    I1 #> I2.
brachylog_greater('integer':0, 'float':I1, 'integer':I2) :-
    nonvar(I1),
    brachylog_label('default', 'integer':I2, _),
    I1 > I2.
brachylog_greater('integer':0, 'integer':I1, 'float':I2) :-
    nonvar(I2),
    brachylog_label('default', 'integer':I1, _),
    I1 > I2.
brachylog_greater('integer':0, 'float':I1, 'float':I2) :-
    nonvar(I1),
    nonvar(I2),
    I1 > I2.
brachylog_greater('integer':1, [], []).
brachylog_greater('integer':1, ['integer':I], ['integer':I]).
brachylog_greater('integer':1, ['integer':I,'integer':J|T], ['integer':I,'integer':J|T]) :-
    I #> J,
brachylog_greater('integer':1, ['integer':J|T], ['integer':J|T]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_TRANSPOSE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_transpose_reversed(S, I, O) :-
    brachylog_transpose(S, O, I).
brachylog_transpose('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_transpose('integer':I, Arg, Output).
brachylog_transpose('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_transpose('integer':I, Arg, Output).
brachylog_transpose('default', Input, Output) :-
    brachylog_transpose('integer':0, Input, Output).
brachylog_transpose('integer':0, 'string':Input, 'string':Output) :-
    brachylog_split_lines('default', Input, Ls),
    brachylog_elements('default', Ls, LLs),
    brachylog_transpose('integer':0, LLs, CCs),
    maplist(brachylog_concatenate('default'), CCs, Cs),
    brachylog_split_lines('default', Output, Cs).
brachylog_transpose('integer':0, Input, Output) :-
    is_brachylog_list(Input),
    member(X, Input),
    is_list(X),
    !,
    maplist(is_brachylog_list, Input),
    length(X, LX),
    length(Input, LI),
    brachylog_juxtapose('integer':LI, ['integer':LX], Lengths),
    maplist(brachylog_length('default'), Input, Lengths),
    brachylog_equal('default', Lengths, _),
    transpose(Input, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_POWER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_power_reversed(S, I, O) :-
    brachylog_power(S, O, I).
brachylog_power('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_power('integer':I, Arg, Output).
brachylog_power('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_power('integer':I, Arg, Output).
brachylog_power('default', [], 'integer':1).
brachylog_power('default', ['integer':I1,'integer':I2], 'integer':Power) :-
    Power #= I1 ^ I2.
brachylog_power('default', ['float':I1,'integer':I2], 'float':Power) :-
    nonvar(I1),
    brachylog_label('default', 'integer':I2, _),
    Power is I1 ^ I2.
brachylog_power('default', ['integer':I1,'float':I2], 'float':Power) :-
    nonvar(I2),
    brachylog_label('default', 'integer':I1, _),
    Power is I1 ^ I2.
brachylog_power('default', ['float':I1,'float':I2], 'float':Power) :-
    nonvar(I1),
    nonvar(I2),
    Power is I1 ^ I2.
brachylog_power('integer':S, 'integer':I, 'integer':J) :-
    J #= I^S.
brachylog_power('integer':S, 'float':I, 'float':J) :-
    nonvar(I),
    brachylog_label('default', 'integer':S, _),
    J is I^S.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_ADFIX
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_adfix_reversed(S, I, O) :-
    brachylog_adfix(S, O, I).
brachylog_adfix('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_adfix('integer':I, Arg, Output).
brachylog_adfix('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_adfix('integer':I, Arg, Output).
brachylog_adfix('default', Input, Output) :-
    (   brachylog_adfix('integer':0, Input, Output)
    ;   brachylog_adfix('integer':1, Input, Output)
    ).
brachylog_adfix('integer':0, [], []).
brachylog_adfix('integer':0, [H|T], [H2|T2]) :-
    (   brachylog_concatenate('default', [[H2|T2],[_|_]], [H|T])
    ;   [H2|T2] = [H|T]
    ).
brachylog_adfix('integer':0, 'string':S, 'string':P) :-
    brachylog_adfix('integer':0, S, P).
brachylog_adfix('integer':0, 'integer':0, 'integer':0).
brachylog_adfix('integer':0, 'integer':I, 'integer':P) :-
    H #\= 0,
    H2 #\= 0,
    abs(P) #=< abs(I),
    integer_value('integer':Sign:[H|T], I),
    integer_value('integer':Sign:[H2|T2], P),
    brachylog_adfix('integer':0, [H|T], [H2|T2]).
brachylog_adfix('integer':1, [], []).
brachylog_adfix('integer':1, [H|T], [H2|T2]) :-
    brachylog_concatenate('default', [_,[H2|T2]], [H|T]).
brachylog_adfix('integer':1, 'string':S, 'string':P) :-
    brachylog_adfix('integer':1, S, P).
brachylog_adfix('integer':1, 'integer':0, 'integer':0).
brachylog_adfix('integer':1, 'integer':I, 'integer':P) :-
    H #\= 0,
    H2 #\= 0,
    abs(P) #=< abs(I),
    integer_value('integer':Sign:[H|T], I),
    integer_value('integer':Sign:[H2|T2], P),
    brachylog_adfix('integer':1, [H|T], [H2|T2]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_BEHEAD
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_behead_reversed(S, I, O) :-
    brachylog_behead(S, O, I).
brachylog_behead('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_behead('integer':I, Arg, Output).
brachylog_behead('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_behead('integer':I, Arg, Output).
brachylog_behead('integer':0, Input, Input).
brachylog_behead('default', Input, Output) :-
    brachylog_behead('integer':1, Input, Output).
brachylog_behead('integer':1, 'string':[_|T], 'string':T).
brachylog_behead('integer':1, 'integer':0, 'integer':0).
brachylog_behead('integer':1, 'integer':I, 'integer':J) :-
    H #\= 0,
    integer_value('integer':Sign:[H|T], I),
    integer_value('integer':Sign:T, J).
brachylog_behead('integer':1, 'float':F,'float':G) :-
    number_codes(F,L),
    brachylog_behead_float(L,M),
    number_codes(G,M).
brachylog_behead('integer':1, [_|T],T).
brachylog_behead('integer':I, Input, Output) :-
    I #> 1,
    brachylog_meta_iterate(ignore, 'integer':I, brachylog_behead, 'integer':1, Input, Output).

brachylog_behead_float([], []).
brachylog_behead_float([48|T], [48|T2]) :-
    brachylog_behead_float(T, T2).
brachylog_behead_float([46|T], [46|T2]) :-
    brachylog_behead_float(T, T2).
brachylog_behead_float([H|T], [48|T2]) :-
    H \= 46,
    H \= 48,
    brachylog_behead_float_(T, T2).

brachylog_behead_float_([], []).
brachylog_behead_float_([H|T], [H|T2]) :-
    brachylog_behead_float_(T, T2).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_CONCATENATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_concatenate_reversed(S, I, O) :-
    brachylog_concatenate(S, O, I).
brachylog_concatenate('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_concatenate('integer':I, Arg, Output).
brachylog_concatenate('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_concatenate('integer':I, Arg, Output).
brachylog_concatenate('default', Input, Output) :-
    brachylog_concatenate('integer':0, Input, Output).
brachylog_concatenate('integer':I, Input, Output) :-
    I #> 0,
    length(Input, I),
    brachylog_concatenate('integer':0, Input, Output).
brachylog_concatenate('integer':0, [], []).
brachylog_concatenate('integer':0, [H|T],L) :-
    var(L),
    (   maplist(is_brachylog_list, [H|T]),
        brachylog_coerce('default', L, L),
        List = L,
        ListOfLists = [H|T],
        Integers = 'no'
    ;   maplist(brachylog_concatenate_prepend_string_or_empty, ListOfLists, [H|T]),
        brachylog_coerce('integer':2, 'string':List, L),
        Integers = 'no'
    ;   maplist(brachylog_concatenate_integer_value, ListOfLists, [H|T]),
        Integers = 'yes'
    ),
    brachylog_concatenate_(ListOfLists, List),
    (   Integers = 'yes',
        List = [0],
        L = 'integer':0
    ;   Integers = 'yes',
        List = [J|TList],
        integer_value('integer':'positive':[J|TList], I),
        L = 'integer':I
    ;   Integers = 'no'
    ).
brachylog_concatenate('integer':0, [H|T], L) :-
    nonvar(L),
    brachylog_length('default', L, 'integer':Length),
    (   var(T) ->
        LengthList #> 0,
        LengthList #=< Length,
        indomain(LengthList),
        length([H|T], LengthList),
        CanContainEmpty = 'no'
    ;   CanContainEmpty = 'yes'
    ),
    (   is_brachylog_list(L),
        maplist(brachylog_coerce('default'), [H|T], [H|T]),
        List = L,
        ListOfLists = [H|T]
    ;   L = 'string':List,
        maplist(brachylog_concatenate_prepend_string_or_empty, ListOfLists, [H|T])
    ;   L = 'integer':I,
        (   I = 0,
            List = [0]
        ;   I #\= 0,
            J #\= 0,
            integer_value('integer':_:[J|TList], I),
            List = [J|TList]
        ),
        (
            CanContainEmpty = 'no' ->
            maplist(brachylog_concatenate_limit_length(1, Length), [H|T])
        ;   maplist(brachylog_concatenate_limit_length(0, Length), [H|T])
        ),
        maplist(brachylog_concatenate_integer_value, ListOfLists, [H|T])
    ),
    (
        CanContainEmpty = 'no' ->
        maplist(brachylog_concatenate_limit_length(1, Length), [H|T])
    ;   maplist(brachylog_concatenate_limit_length(0, Length), [H|T])
    ),
    brachylog_concatenate_(ListOfLists, List).

brachylog_concatenate_prepend_string_or_empty([], []).
brachylog_concatenate_prepend_string_or_empty(S, 'string':S).

brachylog_concatenate_limit_length(Min, Max, H) :-
    Length #>= Min,
    Length #=< Max,
    indomain(Length),
    brachylog_length('default', H, 'integer':Length).

brachylog_concatenate_integer_value([0], 'integer':0).
brachylog_concatenate_integer_value([], []).
brachylog_concatenate_integer_value([H|T], 'integer':I) :-
    H #\= 0,
    integer_value('integer':_:[H|T], I).

brachylog_concatenate_([L|T], L2) :-
    is_brachylog_list(L2),
    append([L|T], L2).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_DUPLICATES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_duplicates_reversed(S, I, O) :-
    brachylog_duplicates(S, O, I).
brachylog_duplicates('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_duplicates('integer':I, Arg, Output).
brachylog_duplicates('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_duplicates('integer':I, Arg, Output).
brachylog_duplicates('default', Input, Output) :-
    brachylog_duplicates('integer':0, Input, Output).
brachylog_duplicates('integer':0, 'string':S, 'string':T) :-
    list_to_set(S, T).
brachylog_duplicates('integer':0, 'integer':I, 'integer':J) :-
    brachylog_label('default', 'integer':I, 'integer':I),
    number_codes(I, C),
    list_to_set(C, S),
    number_codes(J, S).
brachylog_duplicates('integer':0, 'float':F, 'float':G) :-
    number_codes(F, C),
    list_to_set(C, S),
    number_codes(G, S).
brachylog_duplicates('integer':0, L, M) :-
    is_brachylog_list(L),
    list_to_set(L, M).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_FACTORS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_factors_reversed(S, I, O) :-
    brachylog_factors(S, O, I).
brachylog_factors('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_factors('integer':I, Arg, Output).
brachylog_factors('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_factors('integer':I, Arg, Output).
brachylog_factors('default', 'integer':N, Z) :-
    brachylog_label('default', 'integer':N, _),
    (   N = 0 ,
        Z = []
    ;   N #> 0,
        findall('integer':X, (X #>= 1, X #=< N, I #>= 1, I #=< N, N #= X*I, indomain(X)), Z)
    ;   N #< 0,
        findall('integer':X, (X #=< -1, X #>= N, I #>= 1, I #=< abs(N), N #= X*I, labeling([max(X)], [X])), Z)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_GROUP
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_group_reversed(S, I, O) :-
    brachylog_group(S, O, I).
brachylog_group('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_group('integer':I, Arg, Output).
brachylog_group('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_group('integer':I, Arg, Output).
brachylog_group('integer':0, Input, Input).
brachylog_group('default', Input, Output) :-
    brachylog_group('integer':1, Input, Output).
brachylog_group('integer':1, X, [X]).
brachylog_group('integer':I, Input, Output) :-
    I #> 1,
    brachylog_meta_iterate(ignore, 'integer':I, brachylog_group, 'integer':1, Input, Output).

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_HEAD
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_head_reversed(S, I, O) :-
    brachylog_head(S, O, I).
brachylog_head('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_head('integer':I, Arg, Output).
brachylog_head('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_head('integer':I, Arg, Output).
brachylog_head('integer':0, _, []).
brachylog_head('default', 'string':[H|_], 'string':[H]).
brachylog_head('default', 'integer':0, 'integer':0).
brachylog_head('default', 'integer':I, 'integer':J) :-
    J #\= 0,
    integer_value('integer':_:[J|_], I).
brachylog_head('default', 'float':F, 'integer':I) :-
    number_codes(F,L),
    brachylog_head_float(L, 'integer':I).
brachylog_head('default', [H|_], H).
brachylog_head('integer':I, Input, Output) :-
    I #> 0,
    brachylog_length('default', Output, 'integer':I),
    once(brachylog_concatenate('default', [Output, _], Input)).

brachylog_head_float([H|T], 'integer':I) :-
    (   (   H = 48
        ;   H = 46
        ) ->
        (   T = [],
            I = 0
        ;   T \= [],
            brachylog_head_float(T, 'integer':I)    
        )
    ;   H \= 48,
        H \= 46,
        number_codes(I, [H])    
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_INDEX
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_index_reversed(S, I, O) :-
    brachylog_index(S, I, O).
brachylog_index('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_index('integer':I, Arg, Output).
brachylog_index('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_index('integer':I, Arg, Output).
brachylog_index('default', Input, [E,'integer':I]) :-
    brachylog_in('integer':I, Input, E).
brachylog_index('integer':1, Input, [E,'integer':J]) :-
    J #= I + 1,
    brachylog_in('integer':I, Input, E).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_JUXTAPOSE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_juxtapose_reversed(S, I, O) :-
    brachylog_juxtapose(S, O, I).
brachylog_juxtapose('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_juxtapose('integer':I, Arg, Output).
brachylog_juxtapose('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_juxtapose('integer':I, Arg, Output).
brachylog_juxtapose('default', Input, Output) :-
    brachylog_juxtapose('integer':2, Input, Output).
brachylog_juxtapose('integer':_, [], []).
brachylog_juxtapose('integer':0, [_|_], []).
brachylog_juxtapose('integer':I, [H|T], Z) :-
    var(Z),
    Z = [HZ|TZ],
    I #> 0,
    length([H|T], L),
    LZ #= I*L,
    length([HZ|TZ], LZ),
    append([H|T], T2, [HZ|TZ]),
    J #= I-1,
    brachylog_juxtapose('integer':J, [H|T], T2).
brachylog_juxtapose('integer':I, [H|T], Z) :-
    nonvar(Z),
    Z = [HZ|TZ],
    I #> 0,
    length([HZ|TZ], LZ),
    LZ #= I*L,
    indomain(L),
    length([H|T], L),
    append([H|T], T2, [HZ|TZ]),
    J #= I-1,
    brachylog_juxtapose('integer':J, [H|T], T2).
brachylog_juxtapose('integer':I, 'string':S, 'string':Z) :-
    brachylog_juxtapose('integer':I, S, Z).
brachylog_juxtapose('integer':J, 'integer':I, 'integer':Z) :-
    var(Z),
    dif(H, 0),
    integer_value('integer':Sign:[H|T], I),
    brachylog_juxtapose('integer':J, [H|T], LZ),
    LZ = [HZ|_],
    dif(HZ, 0),
    integer_value('integer':Sign:LZ, Z).
brachylog_juxtapose('integer':J, 'integer':I, 'integer':Z) :-
    nonvar(Z),
    dif(H, 0),
    dif(HZ, 0),
    integer_value('integer':Sign:[HZ|TZ], Z),
    brachylog_juxtapose('integer':J, [H|T], [HZ|TZ]),
    integer_value('integer':Sign:[H|T], I).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_KNIFE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_knife_reversed(S, I, O) :-
    brachylog_knife(S, O, I).
brachylog_knife('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_knife('integer':I, Arg, Output).
brachylog_knife('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_knife('integer':I, Arg, Output).
brachylog_knife('integer':0, Input, Input).
brachylog_knife('default', Input, Output) :-
    brachylog_knife('integer':1, Input, Output).
brachylog_knife('integer':1, [_], []).
brachylog_knife('integer':1, [H,I|T], [H2|T2]) :-
    (   var(T) ->
        reverse(T3, [H2|T2]),
        reverse([H,I|T], [_|T3])
    ;   reverse([H,I|T], [_|T3]),
        reverse(T3, [H2|T2])
    ).
brachylog_knife('integer':1, 'string':S, 'string':T) :-
    brachylog_knife('integer':1, S, T).
brachylog_knife('integer':1, 'integer':I, 'integer':0) :-
    I in -9..9.
brachylog_knife('integer':1, 'integer':I, 'integer':J) :-
    H #\= 0,
    H2 #\= 0,
    abs(J) #=< abs(I),
    abs(I) #=< 10*(abs(J) + 1),
    integer_value('integer':Sign:[H|T], I),
    integer_value('integer':Sign:[H2|T2], J),
    brachylog_knife('integer':1, [H|T], [H2|T2]).
brachylog_knife('integer':I, Input, Output) :-
    I #> 1,
    brachylog_meta_iterate(ignore, 'integer':I, brachylog_knife, 'integer':1, Input, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_LENGTH
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_length_reversed(S, I, O) :-
    brachylog_length(S, O, I).
brachylog_length('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_length('integer':I, Arg, Output).
brachylog_length('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_length('integer':I, Arg, Output).
brachylog_length('integer':I, Input, Input) :-
    I #>= 0,
    brachylog_length('default', Input, 'integer':I).
brachylog_length('default', [], 'integer':0).
brachylog_length('default', [H|T], 'integer':Length) :-
    length([H|T], Length).
brachylog_length('default', 'string':S, 'integer':Length) :-
    length(S, Length).
brachylog_length('default', 'integer':0, 'integer':1).
brachylog_length('default', 'integer':I, 'integer':Length) :-
    nonvar(Length),
    (   Length = 1 ->
        I in 1..9
    ;   H #\= 0,
        abs(I) #< 10^Length,
        integer_value('integer':_:[H|T], I),
        length([H|T], Length)
    ).
brachylog_length('default', 'integer':I, 'integer':Length) :-
    var(Length),
    H #\= 0,
    Length #>= 0,
    integer_value('integer':_:[H|T], I),
    length([H|T], Length).
brachylog_length('default', 'float':F, 'integer':Length) :-
    nonvar(F),
    length(L, Length),
    catch(number_codes(F, L), E, (print_message(error, E), false)).
  
  
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_ORDER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_order_reversed(S, I, O) :-
    brachylog_order(S, O, I).
brachylog_order('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_order('integer':I, Arg, Output).
brachylog_order('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_order('integer':I, Arg, Output).
brachylog_order('default', Input, Output) :-
    brachylog_order('integer':0, Input, Output).
brachylog_order('integer':0, 'string':S, 'string':T) :-
    (   nonvar(S),
        msort(S, T)
    ;   var(S),
        msort(T, T),
        brachylog_permute('default', 'string':T, 'string':S)
    ).
brachylog_order('integer':0, 'integer':I, 'integer':J) :-
    brachylog_label('default', 'integer':I, _),
    number_codes(I, C),
    msort(C, D),
    number_codes(J, D).
brachylog_order('integer':0, [], []).
brachylog_order('integer':0, [H|T], [H2|T2]) :-
    (   nonvar(T),
        msort([H|T], [H2|T2])
    ;   var(T),
        msort([H2|T2], [H2|T2]),
        brachylog_permute('default', [H2|T2], [H|T])
    ).
brachylog_order('integer':1, Input, Output) :-
    brachylog_order('integer':0, Input, ROutput),
    brachylog_reverse('default', ROutput, Output).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_PERMUTE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_permute_reversed(S, I, O) :-
    brachylog_permute(S, O, I).
brachylog_permute('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_permute('integer':I, Arg, Output).
brachylog_permute('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_permute('integer':I, Arg, Output).
brachylog_permute('default', Input, Output) :-
    brachylog_permute('integer':0, Input, Output).
brachylog_permute('integer':0, 'string':S, 'string':Permutation) :-
    permutation(S, Permutation).
brachylog_permute('integer':0, List, Permutation) :-
    is_brachylog_list(List),
    is_brachylog_list(Permutation),
    permutation(List, Permutation).
brachylog_permute('integer':0, 'integer':0, 'integer':0).
brachylog_permute('integer':0, 'integer':I, 'integer':J) :-
    H #\= 0,
    J #\= 0,
    integer_value('integer':Sign:[H|L], I),
    permutation([H|L], M),
    integer_value('integer':Sign:M, J).
brachylog_permute('integer':0, 'float':F, 'float':G) :-
    Sign is abs(F)/F,
    AF is abs(F),
    number_chars(AF, C),
    permutation(C, D),
    \+ ( D = ['.'|_] ; reverse(D, ['.'|_])),
    number_chars(AG, D),
    G is Sign*AG.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_SUBSTRING
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_substring_reversed(S, I, O) :-
    brachylog_substring(S, O, I).
brachylog_substring('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_substring('integer':I, Arg, Output).
brachylog_substring('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_substring('integer':I, Arg, Output).
brachylog_substring('default', 'integer':0, 'integer':0).
brachylog_substring('default', 'integer':I, 'integer':J) :-
    H #\= 0,
    integer_value('integer':Sign:[H|L], I),
    brachylog_substring_recur([H|L], [H2|L2]),
    integer_value('integer':Sign:[H2|L2], J).
brachylog_substring('default', 'string':[], 'string':[]).
brachylog_substring('default', 'string':[H|T], 'string':[H2|T2]) :-
    brachylog_substring_recur([H|T], [H2|T2]).
brachylog_substring('default', [], []).
brachylog_substring('default', [H|T], [H2|T2]) :-
    brachylog_substring_recur([H|T], [H2|T2]).
brachylog_substring('integer':I, Input, Output) :-
    brachylog_length('default', Output, 'integer':I),
    brachylog_substring('default', Input, Output).

brachylog_substring_recur([], []).
brachylog_substring_recur([H|T], [H|T2]) :-
    brachylog_substring_recur_(T, T2).
brachylog_substring_recur([_|T], T2) :-
    brachylog_substring_recur(T, T2).

brachylog_substring_recur_([], []).
brachylog_substring_recur_([H|T], [H|T2]) :-
    brachylog_substring_recur_(T, T2).
brachylog_substring_recur_([_|_], []).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_TAIL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_tail_reversed(S, I, O) :-
    brachylog_tail(S, O, I).
brachylog_tail('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_tail('integer':I, Arg, Output).
brachylog_tail('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_tail('integer':I, Arg, Output).
brachylog_tail('integer':0, _, []).
brachylog_tail('default', 'string':L, 'string':[H]) :-
    reverse(L, [H|_]).
brachylog_tail('default', 'integer':0, 'integer':0).
brachylog_tail('default', 'integer':I, 'integer':Z) :-
    J #\= 0,
    integer_value('integer':_:[J|T], I),
    reverse([J|T], [Z|_]).
brachylog_tail('default', 'float':F, 'integer':I) :-
    number_codes(F, L),
    reverse(L, R),
    brachylog_tail_float(R, 'integer':I).
brachylog_tail('default', L, H) :-
    is_brachylog_list(L),
    reverse(L, [H|_]).
brachylog_tail('integer':I, Input, Output) :-
    I #> 0,
    brachylog_length('default', Output, 'integer':I),
    once(brachylog_concatenate('default', [_,Output], Input)).

brachylog_tail_float([H|T], 'integer':I) :-
    (   (   H = 48
        ;   H = 46
        ) ->
        (   T = [],
            I = 0
        ;   T \= [],
            brachylog_tail_float(T, 'integer':I)    
        )
    ;   H \= 48,
        H \= 46,
        number_codes(I, [H])
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_WRITE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_write_reversed(S, I, O) :-
    brachylog_write(S, O, I).
brachylog_write('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_write('integer':I, Arg, Output).
brachylog_write('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_write('integer':I, Arg, Output).
brachylog_write('default', Input, Output) :-
    brachylog_write('integer':0, Input, Output).
brachylog_write('integer':1, [List,'string':F], _) :-
    is_brachylog_list(List),
    atomic_list_concat(F, Format),
    maplist(brachylog_write_try_label, List),
    brachylog_prolog_variable(List, PrologList),
    format(Format, PrologList).
brachylog_write('integer':1, Args, _) :-
    is_brachylog_list(Args),
    reverse(Args, ['string':F|R]),
    reverse(R, S),
    maplist(brachylog_write_try_label, S),
    brachylog_prolog_variable(S, PrologS),
    atomic_list_concat(F, Format),
    format(Format, PrologS).
brachylog_write('integer':0, 'string':S, _) :-
    nonvar(S),
    atomic_list_concat(S, X),
    write(X).
brachylog_write('integer':0, 'integer':I, _) :-
    brachylog_label('default', 'integer':I, _),
    write(I).
brachylog_write('integer':0, 'float':F, _) :-
    nonvar(F),
    write(F).
brachylog_write('integer':0, List, _) :-
    is_brachylog_list(List),
    maplist(brachylog_write_try_label, List),
    brachylog_prolog_variable(List, PrologList),
    write(PrologList).

brachylog_write_try_label(X) :-
    (   nonvar(X), X = 'float':_ -> true
    ;   nonvar(X), X = 'string':_ -> true
    ;   nonvar(X), X = [] -> true
    ;   nonvar(X),
        X = [H|T] ->
        maplist(brachylog_write_try_label, [H|T])
    ;   X = 'integer':I,
        brachylog_label('default', 'integer':I, _)
    ).

   
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_XTERMINATE
   
   TODO: Use sub to know what to remove instead
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_xterminate_reversed(S, I, O) :-
    brachylog_xterminate(S, O, I).
brachylog_xterminate('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_xterminate('integer':I, Arg, Output).
brachylog_xterminate('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_xterminate('integer':I, Arg, Output).
brachylog_xterminate('default', Input, Output) :-
    brachylog_xterminate('integer':0, Input, Output).
brachylog_xterminate('integer':0, [L,[]], L).
brachylog_xterminate('integer':0, ['string':S,X], 'string':T) :-
    \+ is_brachylog_list(X),
    brachylog_xterminate_(X, 'string':S, 'string':T).
brachylog_xterminate('integer':0, ['string':S,[H|T]], L3) :-
    brachylog_xterminate_(H,'string':S, L2),
    brachylog_xterminate('integer':0, [L2,T], L3).
brachylog_xterminate('integer':0, [L,H|T], L3) :-
    is_brachylog_list(L),
    \+ is_brachylog_list(H),
    brachylog_xterminate('integer':0, [L,[H|T]], L3).
brachylog_xterminate('integer':0, [L,[H|T]], L3) :-
    is_brachylog_list(L),
    delete(L, H, L2),
    brachylog_xterminate('integer':0, [L2,T], L3).
    
brachylog_xterminate_(X, 'string':S, 'string':T) :-
    brachylog_xterminate_single(X, 'string':S, 'string':T).
brachylog_xterminate_(_, [], []).
brachylog_xterminate_(X, [H|T], [H2|T2]) :-
    brachylog_xterminate_single(X, H, H2),
    brachylog_xterminate_(X, T, T2).
    
brachylog_xterminate_single('string':L, 'string':H, 'string':Z) :-
    (   append([A,L,B], H),
        append(A,B,H2),
        brachylog_xterminate_single('string':L, 'string':H2, 'string':Z)
    ;   \+ append([_,L,_], H),
        Z = H
    ).
    

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_ZIP
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_zip_reversed(S, I, O) :-
    brachylog_zip(S, O, I).
brachylog_zip('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_zip('integer':I, Arg, Output).
brachylog_zip('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_zip('integer':I, Arg, Output).
brachylog_zip('default', L,Z) :-
    is_brachylog_list(L),
    maplist(brachylog_length('default'), L, Lengths),
    brachylog_order('default', Lengths, OrderedLengths),
    reverse(OrderedLengths, ['integer':MaxLength|_]),
    maplist(brachylog_zip_listify_integer, L, L2),
    brachylog_zip_(L2, MaxLength, Z).
brachylog_zip('integer':0, L, Z) :-
    is_brachylog_list(L),
    maplist(brachylog_length('default'), L, Lengths),
    brachylog_order('default', Lengths, ['integer':MinLength|_]),
    maplist(brachylog_zip_listify_integer, L, L2),
    brachylog_zip_(L2, MinLength, Z).
brachylog_zip('integer':1, L, Z) :-
    is_brachylog_list(L),
    maplist(brachylog_zip_listify_integer, L, L2),
    brachylog_zip_no_cycling(L2, Z).
    
brachylog_zip_(_, 0, []).
brachylog_zip_(L, I, [Heads|Z]) :-
    I #> 0,
    maplist(brachylog_head('default'), L, Heads),
    maplist(brachylog_circular_permute_counterclockwise('default'), L, Tails),
    J #= I - 1,
    brachylog_zip_(Tails, J, Z).

brachylog_zip_no_cycling([H|T], Z) :-
    brachylog_meta_select(ignore, 'default', brachylog_head, 'default', [H|T], Heads),
    (   Heads = [] ->
        Z = []
    ;   Z = [Heads|Z2],
        brachylog_meta_select(ignore, 'default', brachylog_behead, 'default', [H|T], Tails),
        brachylog_zip_no_cycling(Tails, Z2)
    ).

brachylog_zip_listify_integer(L, L) :-
    L \= 'integer':_.
brachylog_zip_listify_integer('integer':I, L) :-
    brachylog_elements('default', 'integer':I, L).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_TO_CODES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_to_codes_reversed(S, I, O) :-
    brachylog_to_codes(S, O, I).
brachylog_to_codes('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_to_codes('integer':I, Arg, Output).
brachylog_to_codes('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_to_codes('integer':I, Arg, Output).
brachylog_to_codes('default', 'string':[], []).
brachylog_to_codes('default', 'string':[H|T], ['integer':I|T2]) :-
    (   var(T) ->
        length(T2,Length),
        length(T,Length)
    ;   length(T,Length),
        length(T2,Length)
    ),
    maplist(prepend_integer, L, ['integer':I|T2]),
    maplist(single_atom_code, [H|T],L).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_BLOCKS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_blocks_reversed(S, I, O) :-
    brachylog_blocks(S, O, I).
brachylog_blocks('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_blocks('integer':I, Arg, Output).
brachylog_blocks('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_blocks('integer':I, Arg, Output).
brachylog_blocks('default', [], []).
brachylog_blocks('default', [H|T], Blocks) :-
    brachylog_blocks('default', [H|T], H, Blocks).
brachylog_blocks('default', 'string':[H|T], StringBlocks) :-
    maplist(prepend_string, Blocks, StringBlocks),
    brachylog_blocks('default', [H|T], H, Blocks),
    !.

brachylog_blocks('default', [], _, [[]]).
brachylog_blocks('default', [H|T], H, [[H|T2]|T3]) :-
    brachylog_blocks('default', T, H, [T2|T3]).
brachylog_blocks('default', [H|T], I, [[],[H|T2]|T3]) :-
    dif(H, I),
    brachylog_blocks('default', T, H, [T2|T3]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_DICHOTOMIZE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_dichotomize_reversed(S, I, O) :-
    brachylog_dichotomize(S, O, I).
brachylog_dichotomize('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_dichotomize('integer':I, Arg, Output).
brachylog_dichotomize('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_dichotomize('integer':I, Arg, Output).
brachylog_dichotomize('default', Input, Output) :-
    brachylog_dichotomize('integer':2, Input, Output).
brachylog_dichotomize('integer':I, Input, Output) :-
    brachylog_label('default', 'integer':I, _),
    length(Output, I),
    brachylog_dichotomize(Input, Output).
brachylog_dichotomize('string':L, L2) :-
    maplist(prepend_string,M, L2),
    brachylog_dichotomize(L, M).
brachylog_dichotomize('integer':0, L2) :-
    length(L2, Length),
    length(M, Length),
    brachylog_dichotomize([0], M),
    maplist(maplist(prepend_integer), M, L2).
brachylog_dichotomize('integer':I, L2) :-
    H #\= 0,
    integer_value('integer':_:[H|T], I),
    length(L2, Length),
    length(M, Length),
    brachylog_dichotomize([H|T], M),
    maplist(maplist(prepend_integer), M, L2).
brachylog_dichotomize(L, L2) :-
    is_brachylog_list(L),
    maplist(is_brachylog_list, L2),
    Length #= LengthL//LengthL2,
    length(L2, LengthL2),
    length(L, LengthL),
    reverse(L2, [_|T]),
    maplist(length_(Length), T),
    append(L2, L),
    !.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_ELEMENTS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_elements_reversed(S, I, O) :-
    brachylog_elements(S, O, I).
brachylog_elements('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_elements('integer':I, Arg, Output).
brachylog_elements('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_elements('integer':I, Arg, Output).
brachylog_elements('default', [], []).
brachylog_elements('default', [H|T], L) :-
    maplist(brachylog_elements('default'), [H|T], L).
brachylog_elements('default', 'string':S, L) :-
    brachylog_meta_find(ignore, 'default', brachylog_in, 'default', 'string':S, L).
brachylog_elements('default', 'integer':I, L) :-
    brachylog_meta_find(ignore, 'default', brachylog_in, 'default', 'integer':I, L).   


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_TO_NUMBER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_to_number_reversed(S, I, O) :-
    brachylog_to_number(S, O, I).
brachylog_to_number('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_to_number('integer':I, Arg, Output).
brachylog_to_number('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_to_number('integer':I, Arg, Output).
brachylog_to_number('default', 'string':S, Type:N) :-
    atomic_list_concat(S, A),
    atom_number(A,N),
    (   member('.', S) ->
        Type = 'float'
    ;   Type = 'integer'
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_LOWERCASE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_lowercase_reversed(S, I, O) :-
    brachylog_lowercase(S, O, I).
brachylog_lowercase('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_lowercase('integer':I, Arg, Output).
brachylog_lowercase('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_lowercase('integer':I, Arg, Output).
brachylog_lowercase('default', 'string':Ls0, 'string':Ls) :-
    maplist(downcase_atom, Ls0, Ls).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_SPLIT_LINES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_split_lines_reversed(S, I, O) :-
    brachylog_split_lines(S, O, I).
brachylog_split_lines('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_split_lines('integer':I, Arg, Output).
brachylog_split_lines('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_split_lines('integer':I, Arg, Output).
brachylog_split_lines('default', 'string':[], ['string':[]]).
brachylog_split_lines('default', 'string':['\n'|T], ['string':[]|T3]) :-
    brachylog_split_lines('default', 'string':T, T3).
brachylog_split_lines('default', 'string':['\r','\r\n'|T], ['string':[]|T3]) :-
    brachylog_split_lines('default', 'string':T, T3).
brachylog_split_lines('default', 'string':[H|T], ['string':[H|T2]|T3]) :-
    dif(H, '\n'),
    dif(H, '\r\n'),
    brachylog_split_lines('default', 'string':T, ['string':T2|T3]).
brachylog_split_lines('integer':1, 'string':[], ['string':[]]).
brachylog_split_lines('integer':1, 'string':[' '|T], ['string':[]|T3]) :-
    brachylog_split_lines('integer':1, 'string':T, T3).
brachylog_split_lines('integer':1, 'string':[H|T], ['string':[H|T2]|T3]) :-
    dif(H, ' '),
    brachylog_split_lines('integer':1, 'string':T, ['string':T2|T3]).
brachylog_split_lines('integer':2, 'string':[], ['string':[]]).
brachylog_split_lines('integer':2, 'string':[' '|T], ['string':[]|T3]) :-
    brachylog_split_lines('integer':2, 'string':T, T3).
brachylog_split_lines('integer':2, 'string':['\n'|T], ['string':[]|T3]) :-
    brachylog_split_lines('integer':2, 'string':T, T3).
brachylog_split_lines('integer':2, 'string':['\r','\r\n'|T], ['string':[]|T3]) :-
    brachylog_split_lines('integer':2, 'string':T, T3).
brachylog_split_lines('integer':2, 'string':[H|T], ['string':[H|T2]|T3]) :-
    dif(H, '\n'),
    dif(H, '\r\n'),
    dif(H, ' '),
    brachylog_split_lines('integer':2, 'string':T, ['string':T2|T3]).
brachylog_split_lines('integer':3, Input, Output) :-
    brachylog_split_lines('default', Input, L1),
    brachylog_meta_map(ignore, 'default', brachylog_split_lines, 'integer':1, L1, Output).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_OCCURENCES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_occurences_reversed(S, I, O) :-
    brachylog_occurences(S, O, I).
brachylog_occurences('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_occurences('integer':I, Arg, Output).
brachylog_occurences('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_occurences('integer':I, Arg, Output).
brachylog_occurences('default', 'string':[], []).
brachylog_occurences('default', 'string':[H|T], L) :-
    brachylog_elements('default', 'string':[H|T], E),
    brachylog_occurences('default', E, L).
brachylog_occurences('default', 'integer':0, [['integer':0,'integer':1]]).
brachylog_occurences('default', 'integer':I, L) :-
    H #\= 0,
    brachylog_elements('default', 'integer':I, ['integer':H|T]),
    brachylog_occurences('default', ['integer':H|T], L).
brachylog_occurences('default', [], []).
brachylog_occurences('default', [H|T], [[H,'integer':O]|T2]) :-
    brachylog_occurences_(H, [H|T], O, R),
    brachylog_occurences('default', R, T2).

brachylog_occurences_(_, [], 0, []).
brachylog_occurences_(H, [H|T], I, R) :-
    I #= J + 1,
    brachylog_occurences_(H, T, J, R).
brachylog_occurences_(H, [H2|T], I, [H2|R]) :-
    dif(H, H2),
    brachylog_occurences_(H, T, I, R).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_RANDOM_ELEMENT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_random_element_reversed(S, I, O) :-
    brachylog_random_element(S, O, I).
brachylog_random_element('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_random_element('integer':I, Arg, Output).
brachylog_random_element('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_random_element('integer':I, Arg, Output).
brachylog_random_element('default', [], []).
brachylog_random_element('default', [H|T], R) :-
    length([H|T], L),
    M #= L - 1,
    random_between(0, M, I),
    nth0(I, [H|T], R).
brachylog_random_element('default', 'string':S, 'string':[R]) :-
    brachylog_random_element('default', S, R).
brachylog_random_element('default', 'integer':0, 'integer':0).
brachylog_random_element('default', 'integer':I, 'integer':R) :-
    H #\= 0,
    integer_value('integer':_:[H|T], I),
    brachylog_random_element('default', [H|T], R).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_SHUFFLE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_shuffle_reversed(S, I, O) :-
    brachylog_shuffle(S, O, I).
brachylog_shuffle('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_shuffle('integer':I, Arg, Output).
brachylog_shuffle('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_shuffle('integer':I, Arg, Output).
brachylog_shuffle('default', [], []).
brachylog_shuffle('default', [H|T], [H2|T2]) :-
    random_permutation([H|T], [H2|T2]).
brachylog_shuffle('default', 'string':S, 'string':T) :-
    brachylog_shuffle('default', S, T).
brachylog_shuffle('default', 'integer':I, 'integer':J) :-
    H #\= 0,
    integer_value('integer':Sign:[H|T], I),
    (   H2 #\= 0,
        brachylog_shuffle('default', [H|T], [H2|T2]) ->
        integer_value('integer':Sign:[H2|T2], J)
    ;   brachylog_shuffle('default', 'integer':I, 'integer':J)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_UPPERCASE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_uppercase_reversed(S, I, O) :-
    brachylog_uppercase(S, O, I).
brachylog_uppercase('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_uppercase('integer':I, Arg, Output).
brachylog_uppercase('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_uppercase('integer':I, Arg, Output).
brachylog_uppercase('default', 'string':Ls0, 'string':Ls) :-
    maplist(upcase_atom, Ls0, Ls).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_WRITELN
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_writeln_reversed(S, I, O) :-
    brachylog_writeln(S, O, I).
brachylog_writeln('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_writeln('integer':I, Arg, Output).
brachylog_writeln('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_writeln('integer':I, Arg, Output).
brachylog_writeln(Sub, X, _) :-
    brachylog_write(Sub, X, _),
    brachylog_write('default', 'string':['\n'], _).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_ABSOLUTE_VALUE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_absolute_value_reversed(S, I, O) :-
    brachylog_absolute_value(S, O, I).
brachylog_absolute_value('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_absolute_value('integer':I, Arg, Output).
brachylog_absolute_value('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_absolute_value('integer':I, Arg, Output).
brachylog_absolute_value('default', 'integer':I, 'integer':J) :-
    J #= abs(I).
brachylog_absolute_value('default', 'float':I, 'float':J) :-
    nonvar(I),
    J is abs(I).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_BASE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_base_reversed(S, I, O) :-
    brachylog_base(S, O, I).
brachylog_base('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_base('integer':I, Arg, Output).
brachylog_base('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_base('integer':I, Arg, Output).
brachylog_base('default', Input, Output) :-
    brachylog_base('integer':2, Input, Output).
brachylog_base('integer':1, 'integer':I, Output) :-
    brachylog_label('default', 'integer':I, _),
    length(Output, I),
    (   Output = ['integer':1|_] ->
        brachylog_equal('default', Output, _)
    ;   true
    ).
brachylog_base('integer':B, 'integer':I, L) :-
    B #> 1,
    %brachylog_label('default', ['integer':I, 'integer':B], _),
    (   I #>= 0
    ;   I #< 0
    ),
    J #= abs(I),
    n_base_digits(J, B, L).

% Credits to @repeat
% See: http://stackoverflow.com/a/33543199/2554145
n_base_digits(Expr, Base, Ds) :-
    Base #> 1,
    Ds = [_|_],
    N #=  Expr,
    N #>= 0,
    n_base_ref_acc_digits(N, Base, Ds, [], Ds).

n_base_ref_acc_digits(N, Base, Ref, Ds0, Ds) :-
    zcompare(Order, N, Base),
    order_n_base_ref_acc_digits(Order, N, Base, Ref, Ds0, Ds).

order_n_base_ref_acc_digits(<, N, _, [_], Ds0, ['integer':N|Ds0]).
order_n_base_ref_acc_digits(=, _, _, [_,_], Ds0, ['integer':1,'integer':0|Ds0]).
order_n_base_ref_acc_digits(>, N, Base, [_|Rs], Ds0, Ds) :-
    N0 #= N //  Base,
    N1 #= N mod Base,
    n_base_ref_acc_digits(N0, Base, Rs, ['integer':N1|Ds0], Ds).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_COERCE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_coerce_reversed(S, I, O) :-
    brachylog_coerce(S, O, I).
brachylog_coerce('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_coerce('integer':I, Arg, Output).
brachylog_coerce('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_coerce('integer':I, Arg, Output).
brachylog_coerce('default', [], []).
brachylog_coerce('default', [H|T], [H|T]).
brachylog_coerce('integer':1, 'integer':I, 'integer':I).
brachylog_coerce('integer':2, 'string':S, 'string':S).



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_PRIME_DECOMPOSITION
   
   Credits to RosettaCode
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_prime_decomposition_reversed(S, I, O) :-
    brachylog_prime_decomposition(S, O, I).
brachylog_prime_decomposition('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_prime_decomposition('integer':I, Arg, Output).
brachylog_prime_decomposition('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_prime_decomposition('integer':I, Arg, Output).
brachylog_prime_decomposition('default', 'integer':N, Z) :-
    N #> 0,
    brachylog_label('default', 'integer':N, _),
    ceiled_square_root(N, SN),
    brachylog_prime_decomposition_1(N, SN, 2, [], L),
    brachylog_prime_decomposition_append_integer(L, Z).
 
brachylog_prime_decomposition_1(1, _, _, L, L) :- !.
brachylog_prime_decomposition_1(N, SN, D, L, LF) :-
    (   0 #= N mod D ->
        Q #= N // D,
        ceiled_square_root(Q, SQ),
        brachylog_prime_decomposition_1(Q, SQ, D, [D|L], LF)
    ;   D1 #= D + 1,
        (   D1 #> SN ->
            LF = [N|L]
        ;   brachylog_prime_decomposition_2(N, SN, D1, L, LF)
        )
    ).
    
brachylog_prime_decomposition_2(1, _, _, L, L) :- !.
brachylog_prime_decomposition_2(N, SN, D, L, LF) :-
    (   0 #= N mod D ->
        Q #= N // D,
        ceiled_square_root(Q, SQ),
        brachylog_prime_decomposition_2(Q, SQ, D, [D|L], LF);
        D1 #= D + 2,
        (   D1 #> SN ->
            LF = [N|L]
        ;   brachylog_prime_decomposition_2(N, SN, D1, L, LF)
        )
    ).

brachylog_prime_decomposition_append_integer([], []).
brachylog_prime_decomposition_append_integer([H|T], ['integer':H|T2]) :-
    brachylog_prime_decomposition_append_integer(T, T2).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_FACTORIAL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_factorial_reversed(S, I, O) :-
    brachylog_factorial(S, O, I).
brachylog_factorial('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_factorial('integer':I, Arg, Output).
brachylog_factorial('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_factorial('integer':I, Arg, Output).
brachylog_factorial('default', 'integer':N, 'integer':F) :-
    brachylog_factorial_(N, 0, 1, F).

brachylog_factorial_(N, I, N0, F) :-
    F #> 0,
    N #>= 0,
    I #>= 0,
    I #=< N,
    N0 #> 0,
    N0 #=< F,
    if_(I #> 2,
        (   F #> N,
            if_(===(N, I, N0, F, T1),
                if_(T1 = true,
                    N0 = F,
                    N = I
                ),
                (   J #= I + 1,
                    N1 #= N0*J,
                    predicates:brachylog_factorial_(N, J, N1, F)
                )
            )
        ),
        if_(N = I,
            N0 = F,
            (   J #= I + 1,
                N1 #= N0*J,
                predicates:brachylog_factorial_(N, J, N1, F)
            )
        )
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_GROUPS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_groups_reversed(S, I, O) :-
    brachylog_groups(S, O, I).
brachylog_groups('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_groups('integer':I, Arg, Output).
brachylog_groups('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_groups('integer':I, Arg, Output).
brachylog_groups('default', Input, Output) :-
    I in 1..sup,
    brachylog_groups('integer':I, Input, Output).
brachylog_groups('integer':I, 'string':Input, Output) :-
    brachylog_groups('integer':I, Input, S),
    maplist(prepend_string, S, Output).
brachylog_groups('integer':I, 'integer':Input, Output) :-
    H #\= 0,
    integer_value('integer':_:[H|T], Input),
    brachylog_groups('integer':I, [H|T], S),
    maplist(brachylog_groups_ints, S, Output).
brachylog_groups('integer':I, Input, Output) :-
    maplist(is_brachylog_list, [Input, Output]),
    brachylog_groups(I, [], Input, Output).

brachylog_groups(_, [H|T], [], [RL]) :- 
    reverse([H|T], RL).
brachylog_groups(I, L, Input, [RL|T2]) :-
    length(L, I),
    reverse(L, RL),
    brachylog_groups(I, [], Input, T2).
brachylog_groups(I, L, [H|T], Output) :-
    J #< I,
    J #>= 0,
    length(L, J),
    brachylog_groups(I, [H|L], T, Output).

brachylog_groups_ints(I, 'integer':O) :-
    integer_value('integer':'positive':I, O).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATRIX
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_matrix_reversed(S, I, O) :-
    brachylog_matrix(S, O, I).
brachylog_matrix('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_matrix('integer':I, Arg, Output).
brachylog_matrix('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_matrix('integer':I, Arg, Output).
brachylog_matrix('default', M, M) :-
    L in 0..sup,
    brachylog_matrix('integer':L, M, M).
brachylog_matrix('integer':L, M, M) :-
    length(M, L),
    maplist(length_(L), M).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_NEGATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_negate_reversed(S, I, O) :-
    brachylog_negate(S, O, I).
brachylog_negate('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_negate('integer':I, Arg, Output).
brachylog_negate('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_negate('integer':I, Arg, Output).
brachylog_negate('default', 'integer':I, 'integer':J) :-
    J #= -I.
brachylog_negate('default', 'float':I, 'float':J) :-
    nonvar(I),
    J is -I.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_PRIME
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_prime_reversed(S, I, O) :-
    brachylog_prime(S, O, I).
brachylog_prime('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_prime('integer':I, Arg, Output).
brachylog_prime('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_prime('integer':I, Arg, Output).
brachylog_prime('default', 'integer':N, 'integer':N) :-
    clpfd:make_propagator(prime(N), Prop),
    clpfd:init_propagator(N, Prop),
    clpfd:trigger_once(Prop).

clpfd:run_propagator(prime(N), MState) :-
    (   nonvar(N) -> 
        clpfd:kill(MState),
        check_prime(N)
    ;   clpfd:fd_get(N, ND, NL, NU, NPs),
        clpfd:cis_max(NL, n(2), NNL),
        clpfd:update_bounds(N, ND, NPs, NL, NU, NNL, NU)
    ).

:- if(current_predicate(crypto_is_prime/2)).
probably_prime(P) :- crypto_is_prime(P, []).
:- else.
probably_prime(_).
:- endif.

check_prime(N) :-
    (   N = 2 -> 
        true
    ;   N #> 2,
        probably_prime(N),
        ceiled_square_root(N, SN),
        check_prime_1(N, SN, 2, [], [_])
    ).

check_prime_1(1, _, _, L, L) :- !.
check_prime_1(N, SN, D, L, LF) :-
    (   0 #= N mod D -> 
        false
    ;   D1 #= D+1,
        (   D1 #> SN ->
            LF = [N|L]
        ;   check_prime_2(N, SN, D1, L, LF)
        )
    ).

check_prime_2(1, _, _, L, L) :- !.
check_prime_2(N, SN, D, L, LF) :-
    (   0 #= N mod D -> false
    ;   D1 #= D+2,
        (   D1 #> SN ->
            LF = [N|L]
        ;   check_prime_2(N, SN, D1, L, LF)
        )
).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_RANDOM_NUMBER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_random_number_reversed(S, I, O) :-
    brachylog_random_number(S, O, I).
brachylog_random_number('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_random_number('integer':I, Arg, Output).
brachylog_random_number('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_random_number('integer':I, Arg, Output).
brachylog_random_number('default', 'integer':I, 'integer':R) :-
    brachylog_random_number(['integer':0,'integer':I], 'integer':R).
brachylog_random_number('default', 'float':I, 'float':R) :-
    brachylog_random_number(['float':0.0,'float':I], 'float':R).
brachylog_random_number('integer':1, _, 'float':R) :-
    brachylog_random_number(['float':0, 'float':1], 'float':R).
brachylog_random_number('integer':2, [A,B], R) :-
    brachylog_random_number([A,B], R).

brachylog_random_number(['integer':I,'integer':J], 'integer':R) :-
    brachylog_label('default', ['integer':I,'integer':J], _),
    (   I #=< J ->
        random_between(I, J, R)
    ;   random_between(J, I, R)
    ).
brachylog_random_number(['float':I,'float':J], 'float':R) :-
    random(K),
    (   I =< J ->
        R is I + K*(J - I)
    ;   R is J + K*(I - J)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_SIGN
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_sign_reversed(S, I, O) :-
    brachylog_sign(S, O, I).
brachylog_sign('first', ['integer':I|Input], Output) :-
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_sign('integer':I, Arg, Output).
brachylog_sign('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_sign('integer':I, Arg, Output).
brachylog_sign('default', 'integer':I, 'integer':S) :-
    (   I = 0,
        S = 0
    ;   I #\= 0,
        S in -1\/1,
        S #= abs(I) // I
    ).
brachylog_sign('default', 'float':F, 'integer':S) :-
    nonvar(F),
    (   F =:= 0.0 -> S = 0
    ;   F > 0.0 -> S = 1
    ;   F < 0.0 -> S = -1
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_TO_STRING
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_to_string_reversed(S, I, O) :-
    brachylog_to_string(S, O, I).
brachylog_to_string('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_to_string('integer':I, Arg, Output).
brachylog_to_string('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_to_string('integer':I, Arg, Output).
brachylog_to_string('default', 'integer':I, 'string':S) :-
    brachylog_label('default', 'integer':I, 'integer':I),
    atom_number(A, I),
    atom_chars(A, S).
brachylog_to_string('default', 'float':F, 'string':S) :-
    atom_number(A, F),
    atom_chars(A, S).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_LABEL
   
   Credits to Markus Triska
   See: http://codereview.stackexchange.com/questions/129924/clpfd-labeling-on-possibly-infinite-domains/129945#129945
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_label_reversed(S, I, O) :-
    brachylog_label(S, O, I).
brachylog_label('first', ['integer':I|Input], Output) :- 
    (   Input = [Arg] -> true
    ;   Input = Arg
    ),
    brachylog_label('integer':I, Arg, Output).
brachylog_label('last', Input, Output) :-
    reverse(Input, ['integer':I|T]),
    (   T = [Arg] -> true
    ;   reverse(T, Arg)
    ),
    brachylog_label('integer':I, Arg, Output).
brachylog_label('default', Input, Output) :-
    brachylog_label('integer':0, Input, Output).
brachylog_label('integer':0, 'integer':Z, 'integer':Z) :-
    unsafe_indomain(Z).
brachylog_label('integer':0, Z, Z) :-
    is_brachylog_list(Z),
    maplist(brachylog_label('integer':0), Z, _).
brachylog_label('integer':1, 'integer':Z, 'integer':Z) :-
    get_time(T),
    T2 is ceil(1000000 * T),
    labeling([random_value(T2)], [Z]).
brachylog_label('integer':1, Z, Z) :-
    is_brachylog_list(Z),
    get_time(T),
    T2 is ceil(1000000 * T),
    labeling([random_value(T2)], Z).

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
    ;   abs(X) #= abs(N),
        positive_integer(N),
        ( X #= N ; X #= -N )
    ).
infinite_down(n(Up), X ) :-
    (   between(0, Up, X)
    ;   abs(X) #= abs(N),
        positive_integer(N),
        X #= -N
    ).

unsafe_up_(sup, Low, X) :-
    (   between(Low, 0, X)
    ;   positive_integer(X)
    ).
unsafe_up_(n(Up), Low, X) :- between(Low, Up, X).

% See: http://stackoverflow.com/a/39259871/2554145
positive_integer(I) :-
    I #> 0,
    (   var(I) ->
        fd_inf(I, Inf),
        (   I #= Inf
        ;   I #\= Inf,
            positive_integer(I)
        )
    ;   true
    ).
