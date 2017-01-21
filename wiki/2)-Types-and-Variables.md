#Types and Variables

##Variables

Any uppercase latin letter is a variable identifier, initially not unified to anything. There are thus 26 available variable names. These variables and their possible values are not shared between rules.

`?` and `.` are special variables which are the Input and the Output of the current rule they are in.

Variables can be unified by using them as inputs or outputs to predicates or by direcly unifying them with other variables. For example, `IJ` will unify `I` with `J` (or fail if they cannot be unified).

##Lists

Lists are noted in between square brackets `[...]` and elements of a list are separated using `:`. One can also construct a list using `:`. For example, `:2:3.` will unify the Output with the list `[Input:2:3]`. However, `2:3` will not unify the Input with the list `[2:3]` ; it will instead unify the Input with 2 and then construct the list `[2:3]` (`[2:3]` would have unified the Input with `[2:3]`). 

`[]` denotes the empty list.

##Strings

Strings are opened and closed with double quotes `"..."`. `\` will escape double quotes.

##Integers and Floats

Integers and floats are written using the numbers 0 to 9 as expected. The decimal separator is `.`. A floating point number must have at least one digit before and after the decimal separator.

Negative numbers are written using an underscore `_` preceeding the number. This underscore is called the *low minus* in Brachylog.