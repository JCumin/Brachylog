# Brachylog
A Prolog-based Code Golf programming language

Currently in development, nowhere near functional at this point.

--------------------------------------

#Predicates

### `A b Z` - Behead

True when `Z` is the tail of `A` (i.e. `A` minus the first element). Works with both lists and strings.


### `A r Z` - Reverse

True if `Z` is `A` reversed. Works with both lists and strings.


### `A s Z` - Sort

True if `Z` is `A` sorted. `A` must be unified. Works with both lists and strings.


#Variables

### `[A-Z]`

Any uppercase letter is a variable identifier.


### `?` - Input

`?` is a reserved variable name representing the input of the current rule.


### `.` - Output

`.` is a reserved variable name representing the output of the current rule.
