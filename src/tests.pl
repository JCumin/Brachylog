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



:- end_tests(predicates).


% Useful templates
test('_', all(X == [])) :-
    run_from_atom('', _, X).

test('_', fail) :-
    run_from_atom('', _, _).
