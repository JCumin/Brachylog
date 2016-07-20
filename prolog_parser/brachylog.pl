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
:- use_module(predicates).
:- use_module(math_predicates).
:- use_module(string_predicates).
:- use_module(constraint_predicates).


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
   RUN_FROM_ATOM_NO_FILE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
run_from_file_no_file(FilePath) :-
    run_from_file_no_file(FilePath,'ignore','ignore').
run_from_file_no_file(FilePath,Input) :-
    run_from_file_no_file(FilePath,Input, 'ignore').
run_from_file_no_file(FilePath,Input,Output) :-
    read_file(FilePath,Code),
    !,
    run_from_atom_no_file(Code,Input,Output).
    
run_from_files_no_file(FilePath,InputPath,OutputPath) :-
    read_file(FilePath,Code),!,
    (
        read_file(InputPath,Input),
        Input \= ''
        ;
        Input = 'ignore'
    ),!,
    (
        read_file(OutputPath,Output),
        Output \= ''
        ;
        Output = 'ignore'
    ),
    !,
    run_from_atom_no_file(Code,Input,Output).
    
run_from_atom_no_file(Code) :-
    run_from_atom_no_file(Code,'ignore','ignore').
run_from_atom_no_file(Code,Input) :-
    run_from_atom_no_file(Code,Input,'ignore').
run_from_atom_no_file(Atom,Input,Output) :-
    parse_no_file(Atom,Predicates),
    !,
    set_prolog_flag(answer_write_options,[quoted(true),
                                          portray(true),
                                          max_depth(10),
                                          spacing(next_argument),
                                          max_depth(0)]),
    set_prolog_flag(print_write_options,[portray(true),
                                         quoted(true),
                                         numbervars(true),
                                         max_depth(0)]),
    maplist(atomic_list_concat,Predicates,ConcatenatedPredicates),
    maplist(read_term_from_atom_,ConcatenatedPredicates,AssertablePredicates),
    maplist(asserta,AssertablePredicates),
    (
		Input \= 'ignore',
        parse_argument(Input,ParsedInput),
        \+ var(ParsedInput),
        ReportInput = 'no'
        ;
        Input == 'ignore',
        ReportInput = 'no'
        ;
        ReportInput = 'yes',
        true
    ),
    (
        
		Output \= 'ignore',
        parse_argument(Output,ParsedOutput),
        \+ var(ParsedOutput),
        ReportOutput = 'no'
        ;
        Output == 'ignore',
        ReportOutput = 'no'
        ;
        ReportOutput = 'yes',
        true
    ),
    !,
    call(brachylog_main,ParsedInput,ParsedOutput) ->
    (
        ReportInput = 'yes'
        -> brachylog_prolog_variable(ParsedInput,InputProlog)
        ;
        true
    ),
    (
        ReportOutput = 'yes'
        -> brachylog_prolog_variable(ParsedOutput,OutputProlog)
        ;
        true
    ),
    (
        ReportInput = 'yes',
        Bindings = [Input=InputProlog]
        ;
        Bindings = []
    ),
    (
        ReportOutput = 'yes',
        BindingsFinal = [Output=OutputProlog|Bindings]
        ;
        BindingsFinal = Bindings,
        true
    ),
    write('\n'),
    reverse(BindingsFinal,RBindings),
    report_bindings(RBindings),
    (
        ReportInput = 'no',
        ReportOutput = 'no',
        write('true.')
        ;
        true
    )
    ;
    write('\nfalse.').

read_term_from_atom_(A,B) :-
    read_term_from_atom(A,B,[]).

% Credits to M. Triska
% See: http://stackoverflow.com/a/38284692/2554145
report_bindings(NameVars) :-
    phrase(bindings(NameVars),Bs),
    format("~s",[Bs]).

bindings([])           --> [].
bindings([E])          --> name_var(E).
bindings([E1,E2|Rest]) --> name_var(E1),",\n",bindings([E2|Rest]).

name_var(Name=Var) -->
    format_("~w = ~q", [Name,Var]).

format_(Format,Ls) -->
    call(format_codes(Format,Ls)).

format_codes(Format,Ls,Cs0,Cs) :-
    format(codes(Cs0,Cs),Format,Ls).


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
    ),!.

read_file_(File,[]) :-
    at_end_of_stream(File).
read_file_(File,[H|T]) :-
    \+ at_end_of_stream(File),
    get_char(File,H),
    read_file_(File,T).