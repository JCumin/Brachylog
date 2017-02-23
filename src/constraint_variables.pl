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


:- module(constraint_variables, [constraintA/1,
                                 constraintB/1,
                                 constraintC/1,
                                 constraintD/1,
                                 constraintE/1,
                                 constraintF/1,
                                 constraintG/1,
                                 constraintH/1,
                                 constraintI/1,
                                 constraintJ/1, % Unused
                                 constraintK/1, % Unused
                                 constraintL/1, % Unused
                                 constraintM/1,
                                 constraintN/1,
                                 constraintO/1,
                                 constraintP/1,
                                 constraintQ/1, % Unused
                                 constraintR/1,
                                 constraintS/1,
                                 constraintT/1,
                                 constraintU/1, % Unused
                                 constraintV/1, % Unused
                                 constraintW/1,
                                 constraintX/1,
                                 constraintY/1,
                                 constraintZ/1
                                ]).
                       
:- use_module(library(clpfd)).
:- use_module(utils).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   CONSTRAINT[A-Z]/1
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
constraintA(_) :-
    true.


constraintB(_) :-
    true.


constraintC([_,_]).


constraintD('integer':X) :-
    X in 0..9.


constraintE(_) :-
    true.


constraintF(_) :-
    true.


constraintG(_) :-
    true.


constraintH(_) :-
    true.


constraintI('integer':X) :-
    X in inf..sup.


constraintJ(_) :- % Unused
    true.


constraintK(_) :- % Unused
    true.


constraintL(_) :- % Unused
    true.


constraintM(_) :-
    true.


constraintN(_) :-
    true.


constraintO(_) :-
    true.


constraintP(_) :-
    true.


constraintQ(_) :- % Unused
    true.


constraintR(_) :-
    true.


constraintS('string':_).


constraintT([_,_,_]).


constraintU(_) :- % Unused
    true.


constraintV(_) :- % Unused
    true.


constraintW(_) :-
    true.


constraintX(_) :-
    true.


constraintY(_) :-
    true.


constraintZ(_) :-
    true.
