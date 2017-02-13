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


:- module(tokenize, [tokenize/2]).

:- use_module(symbols).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TOKENIZE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
tokenize([], []).
tokenize([' '|T], T2) :-
    tokenize(T, T2).
tokenize([Variable|T], ['variable':VariableName|T2]) :-
    is_variable_character(Variable),
    tokenize_variable([Variable|T], Rest, VariableName),
    tokenize(Rest, T2).
tokenize([Variable|T], ['variable':RealVariable|T2]) :-
    (   is_variable_character_dot_above(Variable) -> true
    ;   is_variable_character_dot_below(Variable)
    ),
    token_variable(Variable, RealVariable),
    tokenize(T, T2).
tokenize([Variable|T], ['variable':RealVariable|T2]) :-
    is_math_constant_character(Variable),
    token_variable(Variable, RealVariable),
    tokenize(T, T2).
tokenize([H|T], ['variable':'Input'|T2]) :-
    is_input_character(H),
    tokenize(T, T2).
tokenize([H|T], ['variable':'Output'|T2]) :-
    is_output_character(H),
    tokenize(T, T2).
tokenize([Modifier,Predicate|T], ['predicate':PredName:Sub|T2]) :-
    is_modifier_character(Modifier),
    \+ (is_variable_character(Predicate)),
    atomic_list_concat([Modifier,Predicate], Pred),
    token_predicate(Pred, PredName),
    tokenize_subscript(T, Rest, Sub),
    tokenize(Rest, T2).
tokenize([Predicate|T], ['predicate':PredName:Sub|T2]) :-
    is_predicate_character(Predicate),
    token_predicate(Predicate, PredName),
    tokenize_subscript(T, Rest, Sub),
    tokenize(Rest, T2).
tokenize([MetaPred|T], ['metapredicate':PredName:Sup|T2]) :-
    is_metapredicate_character(MetaPred),
    token_metapredicate(MetaPred, PredName),
    tokenize_superscript(T, Rest, Sup),
    tokenize(Rest, T2).
tokenize(['"'|T], ['variable':Variable|T2]) :-
    tokenize_string(['"'|T], Rest, Variable),
    tokenize(Rest, T2). 
tokenize(['_',Digit|T], ['variable':Type:N|T2]) :-
    is_digit_character(Digit),
    tokenize_number([Digit|T] ,Rest, Type:X),
    N is -X,
    tokenize(Rest, T2).
tokenize(['_','_'|T], T2) :-
    tokenize(T, T2).
tokenize([Digit|T], ['variable':Type:X|T2]) :-
    is_digit_character(Digit),
    tokenize_number([Digit|T], Rest, Type:X),
    tokenize(Rest, T2).
tokenize(['['|T], ['variable':List|T2]) :-
    tokenize_list(['['|T], Rest, List),
    tokenize(Rest, T2).
tokenize([Modifier,Variable|T], ['variable':RealVariable|T2]) :-
    is_modifier_character(Modifier),
    is_variable_character(Variable),
    token_variable(Modifier:Variable, RealVariable),
    tokenize(T, T2).
tokenize([ControlFlow|T], ['control':ControlFlow|T2]) :-
    is_control_character(ControlFlow),
    tokenize(T, T2).
    

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TOKENIZE_VARIABLE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
tokenize_variable([], [], '').
tokenize_variable([H|T], R, Name) :-
    (   is_variable_character(H) ->
        tokenize_variable(T, R, TName),
        atomic_list_concat([H, TName], Name)
    ;   Name = '',
        R = [H|T]
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TOKENIZE_STRING
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
tokenize_string(['"'|T], Rest, 'string':T2) :-
    tokenize_string_(T, Rest, T2).
    
tokenize_string_([], [], []).
tokenize_string_([X,'"'|Rest], Rest, [X]) :-
    X \= '\\',
    X \= '"',
    Rest \= ['"'|_],
    !.
tokenize_string_(['\\','"'|T], Rest, ['"'|T2]) :-
    tokenize_string_(T, Rest, T2).
tokenize_string_(['"','"'|T], Rest, ['"'|T2]) :-
    tokenize_string_(T, Rest, T2).
tokenize_string_([X|T], Rest, L) :-
    (   X \= '"' ->
        L = [X|T2],
        tokenize_string_(T, Rest, T2)
    ;   Rest = T,
        L = []
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TOKENIZE_NUMBER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */  
tokenize_number(N, Rest, Type:Number) :-
    tokenize_number_(N, Rest, T2),
    (   member('.', T2),
        !,
        Type = 'float'
    ;   Type = 'integer'
    ),
    atomic_list_concat(T2, A),
    atom_number(A, Number).
    
tokenize_number_([], [], []).
tokenize_number_(['.',I|T], Rest, ['.',J|T2]) :-
    is_digit_character(I),
    atom_number(I, J),
    tokenize_integer(T, Rest, T2).
tokenize_number_(['.'], ['.'], []).
tokenize_number_(['.',X|T], ['.',X|T], []) :-
    \+ (is_digit_character(X)).
tokenize_number_([X|T], [X|T], []) :-
    \+ (is_digit_character(X)),
    X \= '.'.
tokenize_number_([I|T], Rest, [J|T2]) :-
    is_digit_character(I),
    atom_number(I, J),
    tokenize_number_(T, Rest, T2).

tokenize_integer([], [], []).
tokenize_integer([I|T], Rest, [J|T2]) :-
    is_digit_character(I),
    atom_number(I, J),
    tokenize_integer(T, Rest, T2).
tokenize_integer([X|T], [X|T], []) :-
    \+ (is_digit_character(X)).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TOKENIZE_LIST
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
tokenize_list(['['|T], Rest, List) :-
    isolate_list(T, L, Rest),
    tokenize(L, List).
    
isolate_list(T, List, Rest) :-
    isolate_list(T, 1, [], L, Rest),
    reverse(L, List).
isolate_list([], _, L, L, []).
isolate_list([']'|T], 1, L, L, T).
isolate_list([']'|T], X, L, M, Rest) :-
    X > 1,
    Y is X - 1,
    isolate_list(T, Y, [']'|L], M, Rest).
isolate_list(['['|T], X, L, M, Rest) :-
    Y is X + 1,
    isolate_list(T, Y, ['['|L], M, Rest).
isolate_list([H|T], X, L, M, Rest) :-
    H \= '[',
    H \= ']',
    isolate_list(T, X, [H|L], M, Rest).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TOKENIZE_SUBSCRIPT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
tokenize_subscript(L, Rest, Sub) :-
    tokenize_subscript_(L, Rest, LSub),
    (   LSub = 'first' ->
        Sub = LSub
    ;   LSub = 'last' ->
        Sub = 'last'
    ;   LSub = [] ->
        Sub = 'default'
    ;   maplist(number_codes, LSub, LC),
        append(LC, C),
        number_codes(ISub, C),
        term_to_atom('integer':ISub, Sub)
    ).

tokenize_subscript_([], [], []).
tokenize_subscript_([H|T], Rest, Ds) :-
    (   is_subscript_character(H, D) ->
        tokenize_subscript_(T, Rest, TDs),
        Ds = [D|TDs]
    ;   is_subscript_parenthesis(H, D) ->
        Rest = T,
        Ds = D
    ;   Rest = [H|T],
        Ds = []
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TOKENIZE_SUPERSCRIPT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
tokenize_superscript(L, Rest, Sup) :-
    tokenize_superscript_(L, Rest, LSup),
    (   LSup = 'first' ->
        Sup = LSup
    ;   LSup = 'last' ->
        Sup = 'last'
    ;   LSup = [] ->
        Sup = 'default'
    ;   maplist(number_codes, LSup, LC),
        append(LC, C),
        number_codes(ISup, C),
        term_to_atom('integer':ISup, Sup)
    ).

tokenize_superscript_([], [], []).
tokenize_superscript_([H|T], Rest, Ds) :-
    (   is_superscript_character(H, D) ->
        tokenize_superscript_(T, Rest, TDs),
        Ds = [D|TDs]
    ;   is_superscript_parenthesis(H, D) ->
        Rest = T,
        Ds = D
    ;   Rest = [H|T],
        Ds = []
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   IS_X_CHARACTER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
is_variable_character(X) :-
    member(X, ['A', 'B', 'C', 'D', 'E',
               'F', 'G', 'H', 'I', 'J', 
               'K', 'L', 'M', 'N', 'O', 
               'P', 'Q', 'R', 'S', 'T', 
               'U', 'V', 'W', 'X', 'Y', 'Z']).
               
is_variable_character_dot_below(X) :-
    member(X, ['Ạ', 'Ḅ', 'Ḍ', 'Ẹ',
               'Ḥ', 'Ị', 'Ḳ', 'Ḷ', 
               'Ṃ', 'Ṇ', 'Ọ', 'Ṛ', 
               'Ṣ', 'Ṭ', 'Ụ', 'Ṿ', 
               'Ẉ', 'Ỵ', 'Ẓ']).

is_variable_character_dot_above(X) :-
    member(X,  ['Ȧ', 'Ḃ', 'Ċ', 'Ḋ', 'Ė',
                'Ḟ', 'Ġ', 'Ḣ', 'İ', 'Ṁ',
                'Ṅ', 'Ȯ', 'Ṗ', 'Ṙ', 'Ṡ',
                'Ṫ', 'Ẇ', 'Ẋ', 'Ẏ', 'Ż']).

is_digit_character(X) :-
    member(X, ['0', '1', '2', '3', '4',
               '5', '6', '7', '8', '9']).

is_predicate_character(X) :-
    member(X, ['≤', '≥', '∈', '∋', '⊆', '⊇',
               '↔', '↕', '↑', '↓', '↰', '↺',
               '↻', '√', '⌉', '⌋', '⟦', '⟧',
               'ℕ', 'ℤ', 'ℝ', '∅', '≠', '≡',
               '÷', '×', '%', '&', '*', '+',
               '-', '/', '<', '=', '>', '\\',
               '^', 'a', 'b', 'c', 'd', 'e',
               'f', 'g', 'h', 'i', 'j', 'k',
               'l', 'm', 'n', 'o', 'p', 'q',
               'r', 's', 't', 'u', 'v', 'w',
               'x', 'y', 'z', 'ạ', 'ḅ', 'ḍ',
               'ẹ', 'ḥ', 'ị', 'ḳ', 'ḷ', 'ṃ',
               'ṇ', 'ọ', 'ṛ', 'ṣ', 'ṭ', 'ụ',
               'ṿ', 'ẉ', 'ỵ', 'ẓ', 'ȧ', 'ḃ',
               'ċ', 'ḋ', 'ė', 'ḟ', 'ġ', 'ḣ',
               'ṁ', 'ṅ', 'ȯ', 'ṗ', 'ṙ', 'ṡ',
               'ṫ', 'ẇ', 'ẋ', 'ẏ', 'ż', '≜']).

is_math_constant_character(X) :-
    member(X, ['π', 'φ']).

is_modifier_character(X) :-
    member(X, ['$', '@', '#']).

is_input_character('?').

is_output_character('.').

is_metapredicate_character(X) :-
    member(X, ['ᵃ', 'ᵇ', 'ᶜ', 'ᵈ', 'ᵉ',
               'ᶠ', 'ᵍ', 'ʰ', 'ⁱ', 'ʲ',
               'ᵏ', 'ˡ', 'ᵐ', 'ⁿ', 'ᵒ',
               'ᵖ', 'ʳ', 'ˢ', 'ᵗ', 'ᵘ',
               'ᵛ', 'ʷ', 'ˣ', 'ʸ', 'ᶻ']).

is_subscript_character(C, D) :-
    nth0(D, ['₀','₁','₂','₃','₄',
             '₅','₆','₇','₈','₉'], C).

is_subscript_parenthesis('₍', 'first').
is_subscript_parenthesis('₎', 'last').

is_superscript_character(C, D) :-
    nth0(D, ['⁰','¹','²','³','⁴',
             '⁵','⁶','⁷','⁸','⁹'], C).

is_superscript_parenthesis('⁽', 'first').
is_superscript_parenthesis('⁾', 'last').

is_control_character(X) :-
    member(X, ['∧', '∨', '⊥', '\n', '!',
              '\'', '(', ')', ',', ':',
               ':', '|', '{', '}', '`',
               '¬', '~', ';']).
