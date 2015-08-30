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

### `!` - Cut

Equivalent to Prolog's Cut.

### `\` - Backtrack

`\` is false and will therefore trigger backtracking, if backtracking is possible.

### `( )` - Parentheses

Equivalent to Prolog's parentheses.

The variable preceding (implicitly or not) the opening parenthesis is implicitly available right after the opening parenthesis.

### `'exemple'` - Inline Prolog

Since Brachylog is still a work in progress, and since there are a lot of different useful built-in predicates in SWI-Prolog that we won't/can't fit into one letter predicates, We provide a way to input SWI-Prolog code directly in a Brachylog program. All code between two single-quotes `'` will not be analyzed as Brachylog code and will be outputted as is. Variables names (i.e. uppercase characters) used in those Prolog blocks are shared with the Brachylog program.

For example, `Y'sum_list(Y,Z)'Z.` will unify the Output with the sum of the elements in the Input.


#Arithmetic

There are 6 basic arithmetic operators: `+`, `-`, `*`, `/`, `^` and `%` (addition, subtraction, multiplication, float division, power and modulo).

**Arithmetic expressions are not evaluated automatically**. That is, `5*(3+7)` is not automatically evaluated to `50`. To evaluate arithmetic expressions, one must call the predicate `A = Z` which will attempt to unify `Z` with the evaluation of `A`.

Parentheses can be used, although it can be tricky in some situations. For instance, the program `&(2+3)*5=.`, which unifies the Output with the result of `(2+3)*5` does not actually work, because the parentheses here are considered to be execution control parentheses, and not part of an arithmetic expression. A workaround for this is to rewrite the program as such: `&:(2+3)*5h=.`. Inserting the expression in a list, and then taking the head of the list will give the correctly bracketed expression. Another solution is to simply move parts of the expression so that it does not start with a parenthesis, e.g. `&5*(2+3)=.`. Ultimately, you can avoid parentheses problems in arithmetic expressions as long as any opening parenthesis is preceded by either a colon `:` or an arithmetic operator (`+`, `-`, `*`, `/`, `^` or `%`).


#Predicates

### `A b Z` - Behead

True when `Z` is the tail of `A` (i.e. `A` minus the first element). Works on lists, numbers, strings and atoms.

### `A c Z` - Concatenate

True when `Z` is the concatenation of the elements of list `A`. Works on lists of lists, lists of numbers, lists of strings and lists of atoms. All elements of `A` must be of identical type.

### `A e Z` - Enumerate

If `A = [I,J]` where `I` and `J` are two integers, and with `I <= J`, then `Z` will be successively bound to integers between `I` and `J` (those two bounds included). That is, it will first be unified with `I`, and if backtracking occurs and comes back to this predicate, it will unify `Z` with `I + 1`, etc. up to `J`, after which the predicate will fail.

If `A` is a string, the same thing happens except `Z` is successively bound to each character of `A`.

### `A h Z` - Head

True when Z is the head of `A` (i.e. the first element). Works on lists, numbers, strings and atoms. 

### `A l Z` - Length

True when `Z` is the length of `A`. Works on lists, numbers, strings and atoms.

### `A r Z` - Reverse

True if `Z` is `A` reversed. Works ont lists, numbers, strings and atoms.
