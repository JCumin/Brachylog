#Execution Control

##`,` - Logical *and*

Logical and is implicit in Brachylog. The comma is thus used mostly to *break implicit unification*.

For instance, `42` unifies both the Input and the Output with 42, whereas `,42` will only unify the Output with 42.

##`;` - Logical *or*

As its name suggest, `;` is used to denote logical or. Unlike `|` (to create a new rule, which is also a disjunction), the Input is not implicitely available after it. In fact, `;` breaks implicit unification like `,`.

##`( )` - Parentheses

Parentheses are used to group logical assertions together. The variable preceeding the opening parenthesis is implicitly available after it.

Parentheses are not used in arithmetic expressions.

##`\` - Backtrack

`\` is false, and will therefore trigger backtracking.

##`!` - Cut

Ignore all preceeding choice point, which will thus not get explored when backtracking is triggered. This is equivalent to [Prolog's cut (`!`)](http://www.swi-prolog.org/pldoc/doc_for?object=!/0).

##`` ` `` - Soft cut

Allows the construct ``<if>`<then>;<else>``. If `<if>` is true, then `<then>` is chosen and `<else>` is discarded, meaning you will at this point not reach `<else>` even when backtracking. You will however be able to backtrack on `<if>` and `<then>`. This is equivalent to [Prolog's soft cut (`*->`)](http://www.swi-prolog.org/pldoc/doc_for?object=(*-%3E)/2).

##`'` - Not Provable

True if the following assertion (a built-in predicate, or predicates contained in parentheses, or a predicate definition, or another variable which would unify with the current one) cannot be proven. This is equivalent to Prolog's `\+`.

##`~` - Reverse Arguments

The next predicate will be called with the variable to its left as Output and the variable to its right as Input.

For example, `l` will unify the Output with the length of the Input, whereas `~l` will unify the Output with a list of N elements, if Input = N.