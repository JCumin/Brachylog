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


:- consult(brachylog).


:- begin_tests(predicates).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_LESSEQUAL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('lessequal_1', all(X == [2])) :-
    run_from_atom('1≤2', _, X).
test('lessequal_2', all(X == [2])) :-
    run_from_atom('_1≤2', _, X).
test('lessequal_3', all(X == [0])) :-
    run_from_atom('0≤0', _, X).
test('lessequal_4', all(X == [13])) :-
    run_from_atom('13≤13', _, X).
test('lessequal_5', all(X == [-42])) :-
    run_from_atom('_42≤_42', _, X).
test('lessequal_6', all(X == [[1,2,3]])) :-
    run_from_atom('[1,2,3]≤₁', _, X).
test('lessequal_7', all(X == [[-1,0,1]])) :-
    run_from_atom('[_1,0,1]≤₁', _, X).
test('lessequal_8', all(X == [[-42,-23,-16]])) :-
    run_from_atom('[_42,_23,_16]≤₁', _, X).
test('lessequal_9', all(X == [[1,1,3],[1,2,3],[1,3,3]])) :-
    run_from_atom('[1,I,3]≤₁', _, X).
test('lessequal_10', fail) :-
    run_from_atom('2≤1', _, _).
test('lessequal_11', fail) :-
    run_from_atom('2≤_1', _, _).
test('lessequal_12', fail) :-
    run_from_atom('[1,3,2]≤₁', _, _).
test('lessequal_13', fail) :-
    run_from_atom('[1,I,_1]≤₁', _, _).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_GREATEREQUAL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('greaterequal_1', all(X == [1])) :-
    run_from_atom('2≥1', _, X).
test('greaterequal_2', all(X == [-1])) :-
    run_from_atom('2≥_1', _, X).
test('greaterequal_3', all(X == [0])) :-
    run_from_atom('0≥0', _, X).
test('greaterequal_4', all(X == [13])) :-
    run_from_atom('13≥13', _, X).
test('greaterequal_5', all(X == [-42])) :-
    run_from_atom('_42≥_42', _, X).
test('greaterequal_6', all(X == [[3,2,1]])) :-
    run_from_atom('[3,2,1]≥₁', _, X).
test('greaterequal_7', all(X == [[1,0,-1]])) :-
    run_from_atom('[1,0,_1]≥₁', _, X).
test('greaterequal_8', all(X == [[-16,-23,-42]])) :-
    run_from_atom('[_16,_23,_42]≥₁', _, X).
test('greaterequal_9', all(X == [[3,1,1],[3,2,1],[3,3,1]])) :-
    run_from_atom('[3,I,1]≥₁', _, X).
test('greaterequal_10', fail) :-
    run_from_atom('1≥2', _, _).
test('greaterequal_11', fail) :-
    run_from_atom('_1≥2', _, _).
test('greaterequal_12', fail) :-
    run_from_atom('[1,3,2]≥₁', _, _).
test('greaterequal_13', fail) :-
    run_from_atom('[_1,I,1]≥₁', _, _).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_CONTAINS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
% Nothing, same code as brachylog_in


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_IN
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('in_1', fail) :-
    run_from_atom('∋', [], _).
test('in_2', fail) :-
    run_from_atom('∋', "", _).
test('in_3', all(X == [0])) :-
    run_from_atom('∋', 0, X).
test('in_4', all(X == [7])) :-
    run_from_atom('∋', 7, X).
test('in_5', all(X == [7])) :-
    run_from_atom('∋', '_7', X).
test('in_6', all(X == [1,0,2,3])) :-
    run_from_atom('∋', 1023, X).
test('in_7', all(X == [1,0,2,3])) :-
    run_from_atom('∋', [1,0,2,3], X).
test('in_8', all(X == [1023,"test"])) :-
    run_from_atom('∋', [1023,"test"], X).
test('in_9', all(X == ["test"])) :-
    run_from_atom('∋', ["test"], X).
test('in_10', all(X == ["t","e","s","t"])) :-
    run_from_atom('∋', "test", X).
test('in_11', all(X == [1])) :-
    run_from_atom('∋₀', 1023, X).
test('in_12', all(X == ["t"])) :-
    run_from_atom('∋₀', "test", X).
test('in_13', all(X == [1023])) :-
    run_from_atom('∋₀', [1023,"test"], X).
test('in_14', all(X == [0])) :-
    run_from_atom('∋₁', 1023, X).
test('in_15', fail) :-
    run_from_atom('∋₄', 1023, _).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_SUPERSET
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
% Nothing, same code as brachylog_subset


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_SUBSET
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('subset_1', all(X == [[]])) :-
    run_from_atom('⊇', [], X).
test('subset_2', all(X == [""])) :-
    run_from_atom('⊇', "", X).
test('subset_3', all(X == [0])) :-
    run_from_atom('⊇', 0, X).
test('subset_4', all(X == [7])) :-
    run_from_atom('⊇', 7, X).
test('subset_5', all(X == [123,12,13,23,1,2,3])) :-
    run_from_atom('⊇', 123, X).
test('subset_6', all(X == [[1,2,3],[1,2],[1,3],[2,3],[1],[2],[3],[]])) :-
    run_from_atom('⊇', [1,2,3], X).
test('subset_7', all(X == ["test","tes","tet","tst","est","te","ts","tt","es","et","st","t","e","s","t",""])) :-
    run_from_atom('⊇', "test", X).
test('subset_8', fail) :-
    run_from_atom('⊇', [1,2,3], [1,5]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_REVERSE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('reverse_1', all(X == [0])) :-
    run_from_atom('↔', 0, X).
test('reverse_2', all(X == [[]])) :-
    run_from_atom('↔', [], X).
test('reverse_3', all(X == [""])) :-
    run_from_atom('↔', "", X).
test('reverse_4', all(X == [321])) :-
    run_from_atom('↔', 123, X).
test('reverse_5', all(X == [321])) :-
    run_from_atom('↔', 1230, X).
test('reverse_6', all(X == ["tset"])) :-
    run_from_atom('↔', "test", X).
test('reverse_7', all(X == [[0,3,2,1]])) :-
    run_from_atom('↔', [1,2,3,0], X).
test('reverse_8', all(X == [[1,2,3,2,1]])) :-
    run_from_atom('↔?', '[1,2,3,I,J]', X).
test('reverse_9', all(X == [["a","b","c","b","a"]])) :-
    run_from_atom('↔?', '["a","b","c",I,J]', X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_CALL_PREDICATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
% TODO


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_CIRCULAR_PERMUTE_COUNTERCLOCKWISE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('circular_permute_counterclockwise_1', all(X == [[]])) :-
    run_from_atom('↺', [], X).
test('circular_permute_counterclockwise_2', all(X == [""])) :-
    run_from_atom('↺', "", X).
test('circular_permute_counterclockwise_3', all(X == [0])) :-
    run_from_atom('↺', 0, X).
test('circular_permute_counterclockwise_4', all(X == [231])) :-
    run_from_atom('↺', 123, X).
test('circular_permute_counterclockwise_5', all(X == ["estt"])) :-
    run_from_atom('↺', "test", X).
test('circular_permute_counterclockwise_6', all(X == [[2,"test",1]])) :-
    run_from_atom('↺', [1,2,"test"], X).
test('circular_permute_counterclockwise_7', all(X == [[4,5,6,1,2,3]])) :-
    run_from_atom('↺₃', [1,2,3,4,5,6], X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_CIRCULAR_PERMUTE_CLOCKWISE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('circular_permute_clockwise_1', all(X == [[]])) :-
    run_from_atom('↻', [], X).
test('circular_permute_clockwise_2', all(X == [""])) :-
    run_from_atom('↻', "", X).
test('circular_permute_clockwise_3', all(X == [0])) :-
    run_from_atom('↻', 0, X).
test('circular_permute_clockwise_4', all(X == [312])) :-
    run_from_atom('↻', 123, X).
test('circular_permute_clockwise_5', all(X == ["ttes"])) :-
    run_from_atom('↻', "test", X).
test('circular_permute_clockwise_6', all(X == [["test",1,2]])) :-
    run_from_atom('↻', [1,2,"test"], X).
test('circular_permute_clockwise_7', all(X == [[4,5,6,1,2,3]])) :-
    run_from_atom('↻₃', [1,2,3,4,5,6], X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_ROOT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('root_1', all(X == [-1,1])) :-
    run_from_atom('√', 1, X).
test('root_2', all(X == [0])) :-
    run_from_atom('√', 0, X).
test('root_3', all(X == [-12,12])) :-
    run_from_atom('√', 144, X).
test('root_4', all(X == [2.23606797749979])) :-
    run_from_atom('√', 5, X).
test('root_5', throws(_)) :-
    run_from_atom('√', '_5', _).
test('root_6', all(X == [2.04939015319192])) :-
    run_from_atom('√', 4.2, X).
test('root_7', all(X == [3])) :-
    run_from_atom('√₃', 27, X).
test('root_8', all(X == [-3])) :-
    run_from_atom('√₃', '_27', X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_CEIL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('ceil_1', fail) :-
    run_from_atom('⌉', [], _).
test('ceil_2', all(X == [3])) :-
    run_from_atom('⌉', [1,2,3], X).
test('ceil_3', all(X == [-5])) :-
    run_from_atom('⌉', '[_9,_5,_13]', X).
test('ceil_4', all(X == ["test"])) :-
    run_from_atom('⌉', ["test",2,3], X).
test('ceil_5', all(X == ["z"])) :-
    run_from_atom('⌉', ["test","z"], X).
test('ceil_6', all(X == [123])) :-
    run_from_atom('⌉₁', 123, X).
test('ceil_7', all(X == [124])) :-
    run_from_atom('⌉₁', 123.45, X).
test('ceil_8', all(X == [-123])) :-
    run_from_atom('⌉₁', '_123.45', X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_FLOOR
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('floor_1', fail) :-
    run_from_atom('⌋', [], _).
test('floor_2', all(X == [1])) :-
    run_from_atom('⌋', [1,2,3], X).
test('floor_3', all(X == [-13])) :-
    run_from_atom('⌋', '[_9,_5,_13]', X).
test('floor_4', all(X == [2])) :-
    run_from_atom('⌋', ["test",2,3], X).
test('floor_5', all(X == ["test"])) :-
    run_from_atom('⌋', ["test","z"], X).
test('floor_6', all(X == [123])) :-
    run_from_atom('⌋₁', 123, X).
test('floor_7', all(X == [123])) :-
    run_from_atom('⌋₁', 123.45, X).
test('floor_8', all(X == [-124])) :-
    run_from_atom('⌋₁', '_123.45', X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_RANGE_ASCENDING
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('range_ascending_1', all(X == [[0]])) :-
    run_from_atom('⟦', 0, X).
test('range_ascending_2', all(X == [[0,1,2,3,4,5]])) :-
    run_from_atom('⟦', 5, X).
test('range_ascending_3', all(X == [[-5,-4,-3,-2,-1,0]])) :-
    run_from_atom('⟦', '_5', X).
test('range_ascending_4', all(X == [[1,2,3,4,5]])) :-
    run_from_atom('⟦₁', 5, X).
test('range_ascending_5', all(X == [[9,10,11,12,13]])) :-
    run_from_atom('⟦₂', [9,13], X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_RANGE_DESCENDING
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('range_descending_1', all(X == [[0]])) :-
    run_from_atom('⟧', 0, X).
test('range_descending_2', all(X == [[5,4,3,2,1,0]])) :-
    run_from_atom('⟧', 5, X).
test('range_descending_3', all(X == [[0,-1,-2,-3,-4,-5]])) :-
    run_from_atom('⟧', '_5', X).
test('range_descending_4', all(X == [[5,4,3,2,1]])) :-
    run_from_atom('⟧₁', 5, X).
test('range_descending_5', all(X == [[13,12,11,10,9]])) :-
    run_from_atom('⟧₂', [9,13], X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_NATURAL_INTEGER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('natural_integer_1', all(X == [0])) :-
    run_from_atom('ℕ', 0, X).
test('natural_integer_2', all(X == [42])) :-
    run_from_atom('ℕ', 42, X).
test('natural_integer_3', fail) :-
    run_from_atom('ℕ', "test", _).
test('natural_integer_4', fail) :-
    run_from_atom('ℕ', '_3', _).
test('natural_integer_5', fail) :-
    run_from_atom('ℕ', [1,2], _).
test('natural_integer_6', fail) :-
    run_from_atom('ℕ', 4.2, _).
test('natural_integer_7', all(X == [42])) :-
    run_from_atom('ℕ₄₂', 42, X).
test('natural_integer_8', fail) :-
    run_from_atom('ℕ₄₂', 41, _).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_INTEGER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('integer_1', all(X == [0])) :-
    run_from_atom('ℤ', 0, X).
test('integer_2', all(X == [42])) :-
    run_from_atom('ℤ', 42, X).
test('integer_3', fail) :-
    run_from_atom('ℤ', "test", _).
test('integer_4', all(X == [-3])) :-
    run_from_atom('ℤ', '_3', X).
test('integer_5', fail) :-
    run_from_atom('ℤ', [1,2], _).
test('integer_6', fail) :-
    run_from_atom('ℤ', 4.2, _).
test('integer_7', all(X == [-42])) :-
    run_from_atom('ℤ₄₂', '_42', X).
test('integer_8', fail) :-
    run_from_atom('ℤ₄₂', '_41', _).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_FLOAT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('float_1', all(X == [4.2])) :-
    run_from_atom('ℝ', 4.2, X).
test('float_2', all(X == [0.0])) :-
    run_from_atom('ℝ', 0.0, X).
test('float_3', all(X == [-4.2])) :-
    run_from_atom('ℝ', '_4.2', X).
test('float_4', fail) :-
    run_from_atom('ℝ', 0, _).
test('float_5', fail) :-
    run_from_atom('ℝ', 42, _).
test('float_6', all(X == [42])) :-
    run_from_atom('ℝ₁', 42, X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_EMPTY
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('empty_1', nondet) :-
    run_from_atom('∅', [], _).
test('empty_2', nondet) :-
    run_from_atom('∅', 0, _).
test('empty_3', nondet) :-
    run_from_atom('∅', "", _).
test('empty_4', nondet) :-
    run_from_atom('∅', 0.0, _).
test('empty_5', fail) :-
    run_from_atom('∅', [1], _).
test('empty_6', fail) :-
    run_from_atom('∅', 1, _).
test('empty_7', fail) :-
    run_from_atom('∅', "a", _).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_DIFFERENT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('different_1', all(X == [12345])) :-
    run_from_atom('≠', 12345, X).
test('different_1', fail) :-
    run_from_atom('≠', 12344, _).
test('different_1', all(X == [[1,2,3]])) :-
    run_from_atom('≠', [1,2,3], X).
test('different_1', fail) :-
    run_from_atom('≠', [1,2,1], _).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_INTEGER_DIVISION
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('integer_division_1', all(X == [3])) :-
    run_from_atom('÷', [6,2], X).
test('integer_division_2', all(X == [3])) :-
    run_from_atom('÷', [7,2], X).
test('integer_division_3', all(X == [-3])) :-
    run_from_atom('÷', '[_6,2]', X).
test('integer_division_1', fail) :-
    run_from_atom('÷', '[6,0]', _).
test('integer_division_1', all(X == [0])) :-
    run_from_atom('÷', '[0,_42]', X).
test('integer_division_1', all(X == [3])) :-
    run_from_atom('÷', [6.2,2], X).
test('integer_division_1', all(X == [2])) :-
    run_from_atom('÷₃', 6, X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MULTIPLY
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('multiply_1', all(X == [1])) :-
    run_from_atom('×', [], X).
test('multiply_2', all(X == [12])) :-
    run_from_atom('×', [6,2], X).
test('multiply_3', all(X == [12])) :-
    run_from_atom('×', [2,6], X).
test('multiply_4', all(X == [-12])) :-
    run_from_atom('×', '[_6,2]', X).
test('multiply_5', all(X == [24])) :-
    run_from_atom('×', [2,3,4], X).
test('multiply_6', all(X == [0])) :-
    run_from_atom('×', '[0,_42]', X).
test('multiply_7', all(X == [12.4])) :-
    run_from_atom('×', [6.2,2], X).
test('multiply_8', all(X == [18])) :-
    run_from_atom('×₃', 6, X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MODULO
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('modulo_1', all(X == [1])) :-
    run_from_atom('%', [4,3], X).
test('modulo_2', all(X == [0])) :-
    run_from_atom('%', [4,2], X).
test('modulo_3', all(X == [4])) :-
    run_from_atom('%', [42,19], X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_EXP
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('exp_1', all(X == [22026.465794806718])) :-
    run_from_atom('*', 10, X).
test('exp_2', all(X == [2.302585092994046])) :-
    run_from_atom('*₁', 10, X).
test('exp_3', all(X == [-0.8390715290764524])) :-
    run_from_atom('*₂', 10, X).
test('exp_4', all(X == [-0.5440211108893698])) :-
    run_from_atom('*₃', 10, X).
test('exp_5', all(X == [0.6483608274590866])) :-
    run_from_atom('*₄', 10, X).
test('exp_6', all(X == [1.0471975511965979])) :-
    run_from_atom('*₅', 0.5, X).
test('exp_7', all(X == [0.5235987755982989])) :-
    run_from_atom('*₆', 0.5, X).
test('exp_8', all(X == [1.4711276743037347])) :-
    run_from_atom('*₇', 10, X).
test('exp_9', all(X == [11013.232920103323])) :-
    run_from_atom('*₈', 10, X).
test('exp_10', all(X == [11013.232874703393])) :-
    run_from_atom('*₉', 10, X).
test('exp_11', all(X == [0.9999999958776927])) :-
    run_from_atom('*₁₀', 10, X).
test('exp_12', all(X == [2.993222846126381])) :-
    run_from_atom('*₁₁', 10, X).
test('exp_13', all(X == [2.99822295029797])) :-
    run_from_atom('*₁₂', 10, X).
test('exp_14', all(X == [0.5493061443340549])) :-
    run_from_atom('*₁₃', 0.5, X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_PLUS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('plus_1', all(X == [3])) :-
    run_from_atom('+', [1,2], X).
test('plus_2', all(X == [2])) :-
    run_from_atom('+', '[_3,5]', X).
test('plus_3', all(X == [-5])) :-
    run_from_atom('+', '[_3,_2]', X).
test('plus_4', all(X == [21])) :-
    run_from_atom('+', [1,2,3,4,5,6], X).
test('plus_5', all(X == [4.6])) :-
    run_from_atom('+', [1.2,3.4], X).
test('plus_6', all(X == [13])) :-
    run_from_atom('+', [13,0,0,0,0], X).
test('plus_7', all(X == [67])) :-
    run_from_atom('+₄₂', 25, X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_MINUS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('minus_1', all(X == [-1])) :-
    run_from_atom('-', [1,2], X).
test('minus_2', all(X == [-8])) :-
    run_from_atom('-', '[_3,5]', X).
test('minus_3', all(X == [2])) :-
    run_from_atom('-', '[_1,_3]', X).
test('minus_4', all(X == [-3])) :-
    run_from_atom('-', [1,2,3,4,5,6], X).
test('minus_5', all(X == [-2.2])) :-
    run_from_atom('-', [1.2,3.4], X).
test('minus_6', all(X == [13])) :-
    run_from_atom('-', [13,0,0,0,0], X).
test('minus_7', all(X == [-17])) :-
    run_from_atom('-₄₂', 25, X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_DIVIDE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('divide_1', all(X == [0.5])) :-
    run_from_atom('/', [1,2], X).
test('divide_2', all(X == [3])) :-
    run_from_atom('/', [6,2], X).
test('divide_3', all(X == [-0.5])) :-
    run_from_atom('/', '[1,_2]', X).
test('divide_4', all(X == [0.1111111111111111])) :-
    run_from_atom('/₁', 9, X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_LESS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('less_1', all(X == [2])) :-
    run_from_atom('1<2', _, X).
test('less_2', all(X == [2])) :-
    run_from_atom('_1<2', _, X).
test('less_3', fail) :-
    run_from_atom('0<0', _, _).
test('less_4', fail) :-
    run_from_atom('13<13', _, _).
test('less_5', fail) :-
    run_from_atom('_42<_42', _, _).
test('less_6', all(X == [[1,2,3]])) :-
    run_from_atom('[1,2,3]<₁', _, X).
test('less_7', all(X == [[-1,0,1]])) :-
    run_from_atom('[_1,0,1]<₁', _, X).
test('less_8', all(X == [[-42,-23,-16]])) :-
    run_from_atom('[_42,_23,_16]<₁', _, X).
test('less_9', all(X == [[1,2,3]])) :-
    run_from_atom('[1,I,3]<₁', _, X).
test('less_10', fail) :-
    run_from_atom('2<1', _, _).
test('less_11', fail) :-
    run_from_atom('2<_1', _, _).
test('less_12', fail) :-
    run_from_atom('[1,3,2]<₁', _, _).
test('less_13', fail) :-
    run_from_atom('[1,I,_1]<₁', _, _).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_EQUAL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('equal_1', all(X == [1111])) :-
    run_from_atom('=', 1111, X).
test('equal_2', all(X == [[]])) :-
    run_from_atom('=', [], X).
test('equal_3', all(X == [0])) :-
    run_from_atom('=', 0, X).
test('equal_4', all(X == [[42,42,42,42]])) :-
    run_from_atom('=', '[X,Y,42,Z]', X).
test('equal_5', fail) :-
    run_from_atom('=', [1,1,1,2,1], _).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_GREATER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('greater_1', all(X == [1])) :-
    run_from_atom('2>1', _, X).
test('greater_2', all(X == [-1])) :-
    run_from_atom('2>_1', _, X).
test('greater_3', fail) :-
    run_from_atom('0>0', _, _).
test('greater_4', fail) :-
    run_from_atom('13>13', _, _).
test('greater_5', fail) :-
    run_from_atom('_42>_42', _, _).
test('greater_6', all(X == [[3,2,1]])) :-
    run_from_atom('[3,2,1]>₁', _, X).
test('greater_7', all(X == [[1,0,-1]])) :-
    run_from_atom('[1,0,_1]>₁', _, X).
test('greater_8', all(X == [[-16,-23,-42]])) :-
    run_from_atom('[_16,_23,_42]>₁', _, X).
test('greater_9', all(X == [[3,2,1]])) :-
    run_from_atom('[3,I,1]>₁', _, X).
test('greater_10', fail) :-
    run_from_atom('1>2', _, _).
test('greater_11', fail) :-
    run_from_atom('_1>2', _, _).
test('greater_12', fail) :-
    run_from_atom('[1,3,2]>₁', _, _).
test('greater_13', fail) :-
    run_from_atom('[_1,I,1]>₁', _, _).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_TRANSPOSE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('transpose_1', all(X == [[[1,4,7],[2,5,8],[3,6,9]]])) :-
    run_from_atom('\\', [[1,2,3],[4,5,6],[7,8,9]], X).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BRACHYLOG_POWER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test('power_1', all(X == [8])) :-
    run_from_atom('^', [2,3], X).
test('power_2', all(X == [-8])) :-
    run_from_atom('^', '[_2,3]', X).
test('power_3', all(X == [1])) :-
    run_from_atom('^', [1,50], X).
test('power_4', all(X == [0])) :-
    run_from_atom('^', [0,42], X).
test('power_5', all(X == [1])) :-
    run_from_atom('^', [7,0], X).
test('power_6', all(X == [49])) :-
    run_from_atom('^₂', 7, X).


:- end_tests(predicates).


% Useful templates
test('_', all(X == [])) :-
    run_from_atom('', _, X).

test('_', fail) :-
    run_from_atom('', _, _).
