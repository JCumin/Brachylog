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


:- consult('brachylog.pl').

:- begin_tests(brachylog).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   REVERSE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
test(reverse_lists_0) :-
    findall(Z,run_from_atom('r.',[1:2:3],Z),L), L == [[3,2,1]].
test(reverse_lists_1) :-
    findall(Z,run_from_atom('r.',[],Z),L), L == [[]].
test(reverse_lists_2) :- 
    findall(Z,run_from_atom('r.',["test"],Z),L), L == [["test"]].
test(reverse_lists_3) :-
    findall(Z,run_from_atom('r.',Z,[1:2:3]),L), L == [[3,2,1]].
    
test(reverse_strings_0) :-
    findall(Z,run_from_atom('r.',"test",Z),L), L == ["tset"].
test(reverse_strings_1) :-
    findall(Z,run_from_atom('r.',"",Z),L), L == [""].
test(reverse_strings_2) :-
    findall(Z,run_from_atom('r.',Z,"test"),L), L == ["tset"].

test(reverse_integers_0) :-
    findall(Z,run_from_atom('r.',1234,Z),L), L == [4321].
test(reverse_integers_1) :-
    findall(Z,run_from_atom('r.',0,Z),L), L == [0].
test(reverse_integers_2) :-
    findall(Z,run_from_atom('r.',Z,53),L), L == [35].
test(reverse_integers_3) :-
    findall(Z,run_from_atom('r.',120,Z),L), L == [21].
test(reverse_integers_4) :-
    findall(Z,run_from_atom('r.','_1234',Z),L), L == [-4321].
test(reverse_integers_5) :-
    findall(Z,run_from_atom('r.',Z,5300),L), L == [35].
test(reverse_integers_6) :-
    findall(Z,run_from_atom('r.',Z,'_13000'),L), L == [-31].
    

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

:- end_tests(brachylog).