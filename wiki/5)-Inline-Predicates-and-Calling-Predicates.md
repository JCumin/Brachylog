#Inline Predicates and Calling Predicates

##Inline Predicates

One can define and call a predicate directly inside another predicate using curvy brackets `{...}`. For example:

    predicate0{predicate1}
    predicate2
    ...

As you can see, since the inline predicate appears before the predicate on the second line, it has a smaller number.

Multiple rules can be written inside an inline predicate just like normal predicates. Inline predicates can contain other inline predicates.

An inline predicate will be queried with the variable to the left of the opening `{` as Input and the variable to the right of the closing `}` as Output.

An inline predicate can be declared inside a list (i.e. just after a colon `:`). In that case, it is defined and numbered as normal, but does not get queried.

##Calling Predicates

The main predicate is the one you query to run your program. To call other predicates, you must use the built-in predicate `& - Call predicate`, which takes as Input a list containing your arguments and the number of the predicate you want to call in the last position. If the last element cannot be used as a predicate number, then this will call the predicate you are currently in.

The output of the predicate you called will be unified with the output of `&`.