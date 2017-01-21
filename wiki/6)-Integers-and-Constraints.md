#Integers and Constraints

By default, and unlike Prolog, all integer operations use the [Constraint Logic Programming on Finite Domains (CLPFD) library of SWI-Prolog](https://github.com/triska/clpfd).

This allows for instance to write something like this (`$r` being square root):

    ,25$r

which makes it so that the Output is either `5` or `-5`, without assigning it a value yet.

Variables with constraints that get in "contact" with floating point numbers (e.g. adding an integer to a float) will cause the constrained variable to get labeled before the operation.

Floating point operations do not use CLPFD.

You can force labelization of a variable or a list of variables using the built-in `=` - equals.

##Infinite Domains

Unlike the standard CLP(FD) library of SWI-Prolog, Brachylog's labeling predicate `=` works on infinite sets.

For example:

    ?- run_from_atom('<.=', 0, Z).
    Z = 1 ;
    Z = 2 ;
    Z = 3 ;
    Z = 4 ;
    Z = 5 ;
    ...

With the standard CLP(FD), you would get the following error:

    ?- Z #> 0, indomain(Z).
    ERROR: Arguments are not sufficiently instantiated

A variable with both `inf` as lower bound and `sup` as upper bound will get unified as such:

    ?- run_from_atom('<~+.=',7,Z). % Output's absolute value is bigger than 7
    Z = 8 ;
    Z = -8 ;
    Z = 9 ;
    Z = -9 ;
    Z = 10 ;
    Z = -10 ;
    Z = 11 ;
    Z = -11 ;
    Z = 12 ;
    ...

