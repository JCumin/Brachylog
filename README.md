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

### `A:B:[C:D]:E` - Lists

Lists' elements are chained with a colon `:`. Sublists can be added using an opening square bracket `[` and a closing square bracket `]`. Brackets are optional for the base list, e.g. `A:[B]:C` and `[A:[B]:C]` are the same. One can create a list with the current variable, e.g. `A l:42` will create the list `[L:42]` where `L` is the length of `A`.
Note that `A l B:42` will first unify `B` with the length of `A`, and then create the list `[B:42]`. The empty list is `q`. You cannot put spaces around `[`,`]` or `:`. A list starting with a sublist as first element must start with a colon, e.g. `,:[1:2]:3` and not `,[1:2]:3`.

### `?` - Input

`?` is a reserved variable name representing the input of the current rule.


### `.` - Output

`.` is a reserved variable name representing the output of the current rule. When placed directly after a number, you must add a space ` ` between the two (since `.` is also used as the decimal separator).

### `q` - Empty list

`q` is a reserved variable name representing the empty list `[]`.


#Execution control

### `,` - And

Logic *and* is implicit when chaining predicates, variables, etc. in Brachylog. `,`'s purpose is to disable implicit unification, e.g. `A D` unifies D with A, whereas `A , D` does not.

### `;` - Or

Logic *or*

### `!` - Cut

Equivalent to Prolog's Cut.

### `\` - Backtrack

`\` is false and will therefore trigger backtracking, if backtracking is possible.

### `( )` - Parentheses

Equivalent to Prolog's parentheses.

The variable preceding (implicitly or not) the opening parenthesis is implicitly available right after the opening parenthesis.

### `` `exemple` `` - Inline Prolog

Since Brachylog is still a work in progress, and since there are a lot of different useful built-in predicates in SWI-Prolog that we won't/can't fit into one letter predicates, We provide a way to input SWI-Prolog code directly in a Brachylog program. All code between two back-quotes `` ` `` will not be analyzed as Brachylog code and will be outputted as is. Variables names (i.e. uppercase characters) used in those Prolog blocks are shared with the Brachylog program.

For example, `` Y`sum_list(Y,Z)`Z.`` will unify the Output with the sum of the elements in the Input.


#Arithmetic

There are 6 basic arithmetic operators: `+`, `-`, `*`, `/`, `^` and `%` (addition, subtraction, multiplication, float division, power and modulo).

**Arithmetic expressions are not evaluated automatically**. That is, `5*(3+7)` is not automatically evaluated to `50`. To evaluate arithmetic expressions, one must call the predicate `A = Z` which will attempt to unify `Z` with the evaluation of `A`.

Parentheses can be used, although it can be tricky in some situations. For instance, the program `,(2+3)*5=.`, which unifies the Output with the result of `(2+3)*5` does not actually work, because the parentheses here are considered to be execution control parentheses, and not part of an arithmetic expression. A workaround for this is to rewrite the program as such: `,:(2+3)*5h=.`. Inserting the expression in a list, and then taking the head of the list will give the correctly bracketed expression. Another solution is to simply move parts of the expression so that it does not start with a parenthesis, e.g. `,5*(2+3)=.`. Ultimately, you can avoid parentheses problems in arithmetic expressions as long as any opening parenthesis is preceded by either a colon `:` or an arithmetic operator (`+`, `-`, `*`, `/`, `^` or `%`).


#Predicates and Rules

A Brachylog program starts in the first rule of the predicate `brachylog_main`. New rules for the same predicate can be added using `|`. The Input is implicitly available after `|`. Variables are not shared between rules. Rules are translated to Prolog in the same order they appear in Brachylog (therefore Prolog will attempt to unify the leftmost Brachylog rule first).

For example, the program `q,"Empty".|,"Not empty".` is translated in Prolog as:

    brachylog_main(Input,Output) :-  
        [] = Input,                  % q
        Output = "Empty".            %  ,"Empty".

    brachylog_main(Input,Output) :-  %           |
        Output = "Not empty".        %            ,"Not empty".
        

Sub-predicates can be defined as well, using `{` to start the definition of one and `}` to end it. When defining a sub-predicate, the current variable to the left of the opening `{` will be used as Input and the variable implicitly available after the closing `}` will be the Output (To avoid calling the predicate where it is defined, simply append a `,` - And before the opening brace). Multiple rules can be defined inside a Predicate, exactly like in `brachylog_main`. Sub-predicates can be defined inside sub-predicates.
Sub-predicates have the name `brachylog_subpred_N`, where `N` is an integer. The 0th predicate is `brachylog_main` and subsequent sub-predicates are numbered one after the other, from left to right. Calling a sub-predicate can be done using the built-in predicate *`&` - Call Sub-predicate*, which requires the sub-predicate number.

For example, the program `q|h{q|(h1;?h0),?b:1&},?b:0&`, which returns true if every sublist of an Input list contains only zeros and ones, and false otherwise, is translated in Prolog as:

    brachylog_main(Input,Output) :-            
        [] = Input.                            % q
    
    brachylog_main(Input,Output) :-            %  |
        brachylog_head(Input, V0),             %   h
        brachylog_subpred_1(V0,V1),            %    {...}
        brachylog_behead(Input, V2),           %         ,?b
        brachylog_call_predicate([V2,0], V3).  %            :0&
    
    
    brachylog_subpred_1(Input,Output) :-       % {
        [] = Input.                               q
    
    brachylog_subpred_1(Input,Output) :-       %   |
        (                                      %    (
        brachylog_head(Input, V0),             %     h
        V0 = 1                                 %      1
        ;                                      %       ;
        brachylog_head(Input, V1),             %        ?h
        V1 = 0                                 %          0
        ),                                     %           )
        brachylog_behead(Input, V2),           %            ,?b
        brachylog_call_predicate([V2,1], V3).  %               :1&}


#Built-in Predicates

### `A b Z` - Behead

True when `Z` is the tail of `A` (i.e. `A` minus the first element). Works on lists, numbers, strings.

### `A c Z` - Concatenate

True when `Z` is the concatenation of the elements of list `A`. Works on lists of lists, lists of numbers, lists of strings. All elements of `A` must be of identical type.

### `A d Z` - Duplicates

True when `Z` is `A` with only the left-most copy of each distinct element (therefore `Z` does not contain any duplicates). Works on lists, numbers, strings.

### `A e Z` - Enumerate

If `A = [I:J]` where `I` and `J` are two integers, and with `I <= J`, then `Z` will be successively bound to integers between `I` and `J` (those two bounds included). That is, it will first be unified with `I`, and if backtracking occurs and comes back to this predicate, it will unify `Z` with `I + 1`, etc. up to `J`, after which the predicate will fail.

If `A` is a string, the same thing happens except `Z` is successively bound to each character of `A`.

### `A f Z` - Findall

True if `Z` is a list of all possible variable bindings for which the `A`th sub-predicate will succeed given one of them as Input.

For example, `,{,"golf":Im?}1f.` will ouput a list of all characters of the string `"golf"`:

    ,                 § Prevents immediate call of the next predicate definition
     {                § Defines predicate 1
      ,"golf":I       § Creates a list that contains the string "golf" and the unbounded variable I
               m?}    § Unifies Input with the I-th character of "golf"
                  1f. § Finds all Inputs of predicate 1 for which it succeeds (that is, every character of "golf")

### `A h Z` - Head

True when Z is the head of `A` (i.e. the first element). Works on lists, numbers, strings. 

### `A l Z` - Length

True when `Z` is the length of `A`. Works on lists, numbers, strings.

### `A m Z` - Member

If `A = [L:I]`, true if `Z` is the element of `L` at index `I` (0-based).

If `A = [L:I:R]`, true if `Z` is the element of `L` at index `I` (0-based) and if `R` is the list `L` minus the element at index `I`.

Works on lists, numbers (WIP, the decimal dot is considered to be an element of the number), strings.

### `A o Z` - Order

True if `Z` is `A` sorted in increasing order. Works on lists, numbers, strings.

### `A p Z` - Permute

True if `Z` is a permutation of `A`. Works on lists, numbers (permutation of digits) and strings (permutation of characters). Backtracking on this predicate will iterate over all possible permutations.

### `A r Z` - Reverse

True if `Z` is `A` reversed. Works on lists, numbers, strings.

### `A w Z` - Write

If `A = [List:Format]`, where `List` is a list and `Format` is a string, this predicate is equivalent to the SWI-Prolog predicate [`format/2`](http://www.swi-prolog.org/pldoc/man?predicate=format/2), i.e. `format(Format,List)`, which will print to `STDOUT` the elements of `A` according to the format of `Format`. `Z` is then unified with `List`.

Otherwise (i.e. `A` is a number or a "normal" list or a string), `A` is printed to `STDOUT` using SWI-Prolog predicate [write/1](http://www.swi-prolog.org/pldoc/doc_for?object=write/1). `Z` is then unified with `A`.

### `[A:Elem] x Z` - Xterminate

True if `Z` is `A` minus all elements of `A` that unify with `Elem` (so it essentially deletes all occurrences of `Elem` from `A`). Works on lists, numbers (deletes digits) and strings (deletes characters).

### `[Arg1:...:Argn:I] & Z` - Call Sub-predicate

True when `Z` is the Output of the `I`th sub-predicate with Input = `[Arg1:...:Argn]`. If there is only one argument (e.g. `Arg:I & Z`), then the predicate is called with Input = `Arg` and not `[Arg]`.

### `A = Z` - Equals

True if `A` is an arithmetic expression that evaluates to `Z`.
