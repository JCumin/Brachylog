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


:- module(math_predicates, [brachylog_math_binary/2,
                            brachylog_math_cos/2,
                            brachylog_math_sin/2,
                            brachylog_math_tan/2,
                            brachylog_math_exp/2,
                            brachylog_math_ln/2,
                            brachylog_math_prime_decomposition/2,
                            brachylog_math_root/2,
                            brachylog_math_factorial/2,
                            brachylog_math_ceil/2,
                            brachylog_math_floor/2,
                            brachylog_math_arccos/2,
                            brachylog_math_arcsin/2,
                            brachylog_math_arctan/2,
                            brachylog_math_cosh/2,
                            brachylog_math_sinh/2,
                            brachylog_math_tanh/2,
                            brachylog_math_argcosh/2,
                            brachylog_math_argsinh/2,
                            brachylog_math_argtanh/2,
                            brachylog_math_transpose/2,
                            brachylog_math_antitranspose/2,
                            brachylog_math_circular_permute_left/2,
                            brachylog_math_circular_permute_right/2,
                            brachylog_math_norm/2,
                            brachylog_math_negate/2,
                            brachylog_math_to_string/2,
                            brachylog_math_random_number/2
                           ]).
                       
:- use_module(library(clpfd)).
:- use_module(utils).
                

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_BINARY
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_binary('integer':I, L) :-
    brachylog_equals('integer':I, _),
    J #= abs(I),
    n_base_digits(J, 2, D),
    maplist(prepend_integer, D, L).

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

order_n_base_ref_acc_digits(<, N, _, [_], Ds0, [N|Ds0]).
order_n_base_ref_acc_digits(=, _, _, [_,_], Ds0, [1,0|Ds0]).
order_n_base_ref_acc_digits(>, N, Base, [_|Rs], Ds0, Ds) :-
    N0 #= N //  Base,
    N1 #= N mod Base,
    n_base_ref_acc_digits(N0, Base, Rs, [N1|Ds0], Ds).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_COS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_cos('integer':I, Cos) :-
    brachylog_equals('integer':I, _),
    Cos is cos(I).
brachylog_math_cos('float':F, Cos) :-
    nonvar(F),
    Cos is cos(F).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_SIN
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_sin('integer':I, Sin) :-
    brachylog_equals('integer':I, _),
    Sin is sin(I).
brachylog_math_sin('float':F, Sin) :-
    nonvar(F),
    Sin is sin(F).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_TAN
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_tan('integer':I, Tan) :-
    brachylog_equals('integer':I, _),
    Tan is tan(I).
brachylog_math_tan('float':F, Tan) :-
    nonvar(F),
    Tan is tan(F).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_EXP
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_exp('integer':I, Exp) :-
    brachylog_equals('integer':I, _),
    Exp is exp(I).
brachylog_math_exp('float':F, Exp) :-
    nonvar(F),
    Exp is exp(F).
    

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_LN
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_ln('integer':I, Ln) :-
    brachylog_equals('integer':I, _),
    Ln is log(I).
brachylog_math_ln('float':F, Ln) :-
    nonvar(F),
    Ln is log(F).
    

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_PRIME_DECOMPOSITION
   
   Credits to RosettaCode
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_prime_decomposition('integer':N, Z) :-
    N #> 0,
    brachylog_equals('integer':N, _),
    ceiled_square_root(N, SN),
    brachylog_math_prime_decomposition_1(N, SN, 2, [], L),
    brachylog_math_prime_decomposition_append_integer(L, Z).
 
brachylog_math_prime_decomposition_1(1, _, _, L, L) :- !.
brachylog_math_prime_decomposition_1(N, SN, D, L, LF) :-
    (   0 #= N mod D ->
        Q #= N // D,
        ceiled_square_root(Q, SQ),
        brachylog_math_prime_decomposition_1(Q, SQ, D, [D|L], LF)
    ;   D1 #= D + 1,
        (   D1 #> SN ->
            LF = [N|L]
        ;   brachylog_math_prime_decomposition_2(N, SN, D1, L, LF)
        )
    ).
    
brachylog_math_prime_decomposition_2(1, _, _, L, L) :- !.
brachylog_math_prime_decomposition_2(N, SN, D, L, LF) :-
    (   0 #= N mod D ->
        Q #= N // D,
        ceiled_square_root(Q, SQ),
        brachylog_math_prime_decomposition_2(Q, SQ, D, [D|L], LF);
        D1 #= D + 2,
        (   D1 #> SN ->
            LF = [N|L]
        ;   brachylog_math_prime_decomposition_2(N, SN, D1, L, LF)
        )
    ).
	
brachylog_math_prime_decomposition_append_integer([], []).
brachylog_math_prime_decomposition_append_integer([H|T], ['integer':H|T2]) :-
	brachylog_math_prime_decomposition_append_integer(T, T2).

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_ROOT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_root('integer':I, Type:R) :-
    brachylog_math_root(['integer':I,'integer':2], Type:R).
brachylog_math_root('float':F, 'float':R) :-
    brachylog_math_root(['float':F,'integer':2], 'float':R).
brachylog_math_root(['integer':I,'integer':E], Type:R) :-
    (   I #= R^E ->
        Type = 'integer'
    ;   label([I,E]),
        R is I^(1/E),
        Type = 'float'
    ).
brachylog_math_root(['integer':I,'float':E], 'float':R) :-
    brachylog_equals('integer':I, _),
    R is I^(1/E).
brachylog_math_root(['float':I,'integer':E], 'float':R) :-
    brachylog_equals('integer':I, _),
    R is I^(1/E).
brachylog_math_root(['float':I,'float':E], 'float':R) :-
    R is I^(1/E).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_FACTORIAL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_factorial('integer':0, 'integer':1).
brachylog_math_factorial('integer':I, 'integer':J) :-
    I #> 0,
    A #= I - 1,
    J #= I * B,
    brachylog_math_factorial('integer':A, 'integer':B).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_CEIL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */    
brachylog_math_ceil('integer':I, 'integer':I).
brachylog_math_ceil('float':I, 'integer':J) :-
    nonvar(I),
    J is ceil(I).
 
 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_FLOOR
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_floor('integer':I, 'integer':I).
brachylog_math_floor('float':I, 'integer':J) :-
    nonvar(I),
    J is floor(I).

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_ARCCOS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_arccos('integer':I, ACos) :-
    brachylog_equals('integer':I, _),
    ACos is acos(I).
brachylog_math_arccos('float':F, ACos) :-
    nonvar(F),
    ACos is acos(F).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_ARCSIN
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_arcsin('integer':I, ASin) :-
    brachylog_equals('integer':I, _),
    ASin is asin(I).
brachylog_math_arcsin('float':F, ASin) :-
    nonvar(F),
    ASin is asin(F).
  
  
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_ARCTAN
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_arctan('integer':I, ATan) :-
    brachylog_equals('integer':I, _),
    ATan is atan(I).
brachylog_math_arctan('float':F, ATan) :-
    nonvar(F),
    ATan is atan(F).    
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_COSH
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_cosh('integer':I, Cosh) :-
    brachylog_equals('integer':I, _),
    Cosh is cosh(I).
brachylog_math_cosh('float':F, Cosh) :-
    nonvar(F),
    Cosh is cosh(F).
  
  
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_SINH
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_sinh('integer':I, Sinh) :-
    brachylog_equals('integer':I, _),
    Sinh is sinh(I).
brachylog_math_sinh('float':F, Sinh) :-
    nonvar(F),
    Sinh is sinh(F).
 
 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_TANH
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_tanh('integer':I, Tanh) :-
    brachylog_equals('integer':I, _),
    Tanh is tanh(I).
brachylog_math_tanh('float':F, Tanh) :-
    nonvar(F),
    Tanh is tanh(F).

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_ARGCOSH
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_argcosh('integer':I, ACosh) :-
    brachylog_equals('integer':I, _),
    ACosh is acosh(I).
brachylog_math_argcosh('float':F, ACosh) :-
    nonvar(F),
    ACosh is acosh(F).
  
  
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_ARGSINH
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_argsinh('integer':I, ASinh) :-
    brachylog_equals('integer':I, _),
    ASinh is asinh(I).
brachylog_math_argsinh('float':F, ASinh) :-
    nonvar(F),
    ASinh is asinh(F).
  
  
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_ARGTANH
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_argtanh('integer':I, ATanh) :-
    brachylog_equals('integer':I, _),
    ATanh is atanh(I).
brachylog_math_argtanh('float':F, ATanh) :-
    nonvar(F),
    ATanh is atanh(F).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_TRANSPOSE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_transpose(List, TransposedList) :-
    is_brachylog_list(List),
    transpose(List, TransposedList).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_ANTITRANPOSE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_antitranspose(List, TransposedList) :-
    is_brachylog_list(List),
    maplist(reverse, List, A),
    transpose(A, B),
    maplist(reverse, B, TransposedList).
 
 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_CIRCULAR_PERMUTE_LEFT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_circular_permute_left('string':[], 'string':[]).
brachylog_math_circular_permute_left('string':[H|T], 'string':S) :-
    append(T, [H], S).
brachylog_math_circular_permute_left([H|T], S) :-
    append(T, [H], S).
brachylog_math_circular_permute_left('integer':I, 'integer':J) :-
    dif(H, 0),
    integer_value('integer':Sign:[H|T], I),
    append(T, [H], S),
    integer_value('integer':Sign:S, J).
 
 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_CIRCULAR_PERMUTE_RIGHT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_circular_permute_right('string':[], 'string':[]).
brachylog_math_circular_permute_right('string':L, 'string':S) :-
    append(T, [H], L),
    S = [H|T].
brachylog_math_circular_permute_right([A|B], S) :-
    append(T, [H], [A|B]),
    S = [H|T].
brachylog_math_circular_permute_right('integer':I, 'integer':J) :-
    dif(H2, 0),
    integer_value('integer':Sign:[H2|T2], I),
    append(T, [H], [H2|T2]),
    S = [H|T],
    integer_value('integer':Sign:S, J).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_NORM
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_norm('integer':I, 'integer':J) :-
    J #= abs(I).
brachylog_math_norm('float':I, 'float':J) :-
    J is abs(I).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_NEGATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_negate('integer':I, 'integer':J) :-
    J #= -I.
brachylog_math_negate('float':I, 'float':J) :-
    J is -I.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_TO_STRING
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_to_string('integer':I, 'string':S) :-
    brachylog_equals('integer':I, 'integer':I),
    atom_number(A, I),
    atom_chars(A, S).
brachylog_math_to_string('float':F, 'string':S) :-
    atom_number(A, F),
    atom_chars(A, S).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MATH_RANDOM_NUMBER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
brachylog_math_random_number('integer':I, 'integer':R) :-
    brachylog_math_random_number(['integer':0,'integer':I], 'integer':R).
brachylog_math_random_number(['integer':I,'integer':J], 'integer':R) :-
    brachylog_equals(['integer':I,'integer':J], _),
    (   I #=< J ->
        random_between(I, J, R)
    ;   random_between(J, I, R)
    ).
brachylog_math_random_number('float':I, 'float':R) :-
    brachylog_math_random_number(['float':0.0,'float':I], 'float':R).
brachylog_math_random_number(['float':I,'float':J], 'float':R) :-
    random(K),
    (   I =< J ->
        R is I + K*(J - I)
    ;   R is J + K*(I - J)
    ).
