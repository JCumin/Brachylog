#Program Structure

##Predicates and rules

A Brachylog program is constituted of *predicates*, each predicate being constituted of *rules*. Each predicate corresponds to one line of code:

    predicate0
    predicate1
    predicate2
    ...

Each predicate has a number, starting from 0, which is based on its order of apparition in the code (reading from left to right and top to bottom). The first predicate, written on the first line (so it has number 0), is called the *main predicate*, and it is the predicate you query to run your code.

A new rule can be added to any predicate using the symbol `|`:

    rule0_pred0|rule1_pred0|rule2_pred0
    rule0_pred1
    ...

Brachylog is built on Prolog, and as such rules in Brachylog behave as you would expect in Prolog. That is, if you query say predicate i, it will attempt to satisfy rule 0 of this predicate, and if unsuccessful attempt to satisfy rule 1 (if it exists), etc., until a rule is satisfied or none can be satisfied.

Each predicate has two arguments: the *Input* and the *Output*. The Input is represented by `?` in the code, and the Output by `.`. At the beginning of each rule, the Input is implicitely the current variable. At the end of each rule, the Output is implicitely here too.

##Syntax and our first Brachylog program

Brachylog, like Prolog, uses *unification* and *backtracking* as basic mechanisms. Once a Brachylog variable is unified with something, it cannot be changed, unless backtracking occurs.

In a rule definition, things get processed from left to right and each built-in predicate will use the variable to its left as Input, and the variable to its right as Output. In Brachylog, logical conjunction (i.e. logical *and*) is *implicit* inside a rule definition. Unification is also implicit. To illustrate this, Let's examine this short Brachylog code containing only a main predicate, which unifies its Output with the reverse of its Input minus the first element:

     br

As said before, the Input is implicitely available at the beginning of a rule. Thus, the built-in predicate `b - Behead ` will use the Input of the rule as its own input. There is no variable between `b` and `r - Reverse`, therefore an implicit one is used as output for `b`, and reused as input for `r`. The Output is implicitely the last variable of at the end of a rule, therefore `r` will use the Output of this rule as its own Output.

Overall, this predicate will be satisfied if and only if an implicit variable can be unified with the Input minus its first element, and then the Output can be unified with the reverse of this implicit variable. This predicate will return `false`otherwise.
