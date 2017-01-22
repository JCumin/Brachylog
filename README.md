<p align="center"><a href="https://github.com/JCumin/Brachylog"><img src="https://github.com/JCumin/Brachylog/blob/master/misc/brachylog_logo.png" alt="Brachylog" width="600"/></a></p>

#What is it?

Brachylog is a declarative logic programming language much like Prolog. Brachylog is designed to be much terser than Prolog, while retaining some readability.

Currently in development.

Brachylog uses [SWI-Prolog](http://www.swi-prolog.org/) as Prolog engine.

##How do I use this?

###The language itself

Check out [Brachylog's Wiki](https://github.com/JCumin/Brachylog/wiki) if you want to learn how to write programs in this language.

###Try it online!

You can try out Brachylog on [Try it online!](https://tio.run/nexus/brachylog2), thanks to @DennisMitchell.

###The interpreter

Brachylog's interpreter is entirely written in Prolog. Therefore, installing [SWI-Prolog](http://www.swi-prolog.org/) (version 7 and up) is mandatory to use Brachylog (We do not guarantee that Brachylog's interpreter will be compatible with any other Prolog engines).

To run Brachylog's interpreter, start SWI-Prolog's interpreter inside the `prolog_parser` directory available in this repository, and consult the file `brachylog.pl` (`consult(brachylog.pl).`).

You can then run Brachylog programs using different predicates:

 - `run_from_file(FileName, Input, Output)`: `FileName` is an atom (i.e. between single quotes `'`) representing the file containing your Brachylog code. For example: `run_from_file('code.brachylog',"Test Input",Z)`.
 
 - `run_from_atom(Code, Input, Output)`: `Code` is an atom (i.e. between single quotes `'`) containing your Brachylog code. For example: `run_from_atom(',"Hello, World!"w',_,_)`. Note that you will have to escape certain characters in `Code`.

 - `run(Input, Output)`: This will run a Brachylog program that has already been transpiled to Prolog using either of the two previous predicates. More precisely, this will query `brachylog_main/2` in the file `compiled_brachylog.pl`.

The first two predicates will transpile your Brachylog program into Prolog, subsequently generating a file called `compiled_brachylog.pl` in the same directory that contains `brachylog.pl`. The three run predicates will then consult it and query `brachylog_main/3`.
 
Note that the first two run predicates also exist with either no `Output` argument, or with no `Input` nor `Output` argument, if necessary. For example, `run_from_file('code.brachylog')` is equivalent to `run_from_file('code.brachylog', _, _)`.
