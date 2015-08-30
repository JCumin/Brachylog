# Brachylog
A Prolog-based Code Golf programming language

Currently in development.
Uses SWI-Prolog as Prolog engine.

#Program structure

*(We assume that the reader has basic knowledge of Prolog in the following)*

A brachylog program is always constitued of a main predicate which has two arguments called Input and Output. In general, every predicate in Brachylog (built-ins included) have one Input and one Output argument. The program is entirely written on one line, and things get processed left to right. Predicates call will use the variable immediatly to the left of the predicate as Input and the variable immediatly to the right of the predicate as Output. Implicit variables are used when chaining predicates. Logical *and* and Unification are mostly implicit in programs as well. 

For example, the program `bArA`, which uses the variable `A`, the built-in predicate `b` - Behead and the built-in predicate `r` - Reverse, will return `true` if its input minus the first element is a palindrome (i.e. is identical to its reverse), and false otherwise. Here is a breakdown of what's happening:

    b     § An implicit variable V0 is unified with Input minus the first element
     A    § Unifies variable A with V0
      r   § An implicit variable V1 is unified with the reverse of A = V0
       A  § Unifies variable A with V1
       
If the Input minus its first element is not a palindrome, the last unification of `A` with `V1` will fail, and thus the main predicate will return false since it is only constitued of implicit logical *ands*.

The equivalent Prolog program, without the implicit variables, is:

    main(Input, Output) :-
        [_|A] = Input,      % bA
        reverse(A,A).       % rA

--------------------------------------

#Types and Variables

### `[A-Z]` - Variables

Any uppercase letter is a variable identifier.

### `"Exemple"` - Strings

Strings are opened and closed with double quotes `"`. `\` will escape the following character.

### `12345.67890` - Numbers

Numbers consist of characters `0` to `9`. Floats are written with a period `.` as decimal separator.

### `A:B:[C:D]:E` -Lists

Lists' elements are chained with a colon `:`. Sublists can be added using an opening square bracket `[` and a closing square bracket `]`. Brackets are optional for the base list, e.g. `A:[B]:C` and `[A:[B]:C]` are the same. One can create a list with the current variable, e.g. `A l:42` will create the list `[L:42]` where `L` is the length of `A`.
Note that `A l B:42` will first unify `B` with the length of `A`, and then create the list `[B:42]`. The empty list is `[]`. You cannot put spaces around `[`,`]` or `:`.

### `?` - Input

`?` is a reserved variable name representing the input of the current rule.


### `.` - Output

`.` is a reserved variable name representing the output of the current rule. When placed directly after a number, you must add a space ` ` between the two (since `.` is also used as the decimal separator).



#Execution control

### `&` - And

Logic *and* is implicit when chaining predicates, variables, etc. in Brachylog. `&` purpose is to disable implicit unification, e.g. `A D` unifies D with A, whereas `A & D` does not.

### `;` - Or

Logic *or*

#Predicates

### `A b Z` - Behead

True when `Z` is the tail of `A` (i.e. `A` minus the first element). Works on lists, numbers, strings and atoms.

### `A l Z` - Length

True when `Z` is the length of `A`. Works on lists, numbers, strings and atoms.

### `A r Z` - Reverse

True if `Z` is `A` reversed. Works ont lists, numbers, strings and atoms.
