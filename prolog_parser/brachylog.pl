/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
____            ____
\   \          /   /
 \   \  ____  /   /
  \   \/    \/   /
   \     /\     /     BRACHYLOG       
    \   /  \   /      A terse declarative logic programming language
    /   \  /   \    
   /     \/     \     Written by Julien Cumin - 2016
  /   /\____/\   \    https://github.com/JCumin/Brachylog
 /   /  ___   \   \
/___/  /__/    \___\
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- use_module(transpile).
:- use_module(symbols).
:- use_module(utils).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   RUN_FROM_FILE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
run_from_file(FilePath) :-
    run_from_file(FilePath,'ignore','ignore').
run_from_file(FilePath,Input) :-
    run_from_file(FilePath,Input, 'ignore').
run_from_file(FilePath,Input,Output) :-
    read_file(FilePath,Code),
    !,
    run_from_atom(Code,Input,Output).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   RUN_FROM_ATOM
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
run_from_atom(Code) :-
    run_from_atom(Code,'ignore','ignore').
run_from_atom(Code,Input) :-
    run_from_atom(Code,Input,'ignore').
run_from_atom(Code,Input,Output) :-
    parse(Code,'compiled_brachylog.pl'),
    !,
    run(Input,Output).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   RUN
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
run(Input,Output) :-
    set_prolog_flag(answer_write_options,[quoted(true),
                                          portray(true),
                                          max_depth(10),
                                          spacing(next_argument),
                                          max_depth(0)]),
    set_prolog_flag(print_write_options,[portray(true),
                                         quoted(true),
                                         numbervars(true),
                                         max_depth(0)]),
    consult('compiled_brachylog.pl'),
    (
        \+ var(Input),
		Input \= 'ignore',
        parse_argument(Input,ParsedInput)
        ;
        true
    ),
    (
        \+ var(Output),
		Output \= 'ignore',
        parse_argument(Output,ParsedOutput)
        ;
        true
    ),
    !,
    call(brachylog_main,ParsedInput,ParsedOutput),
    (
        var(Input)
        -> brachylog_prolog_variable(ParsedInput,Input)
        ;
        true
    ),
    (
        var(Output)
        -> brachylog_prolog_variable(ParsedOutput,Output)
        ;
        true
    ).
    
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   READ_FILE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
read_file(FilePath,Code) :-
    (
        exists_file(FilePath),
        open(FilePath,read,File),
        read_file_(File,Chars),
        close(File),
        atomic_list_concat(Chars,Code)
        ;
        throw('The file does not exist.')
    ).

read_file_(File,[]) :-
    at_end_of_stream(File).
read_file_(File,[H|T]) :-
    \+ at_end_of_stream(File),
    get_char(File,H),
    read_file_(File,T).
