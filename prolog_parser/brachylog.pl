:- use_module(transpile).

/*
RUN_FROM_FILE
*/
run_from_file(FilePath) :-
	run_from_file(FilePath,_,_).
run_from_file(FilePath,Input) :-
	run_from_file(FilePath,Input, _).
run_from_file(FilePath,Input,Output) :-
	read_file(FilePath,Code),
	run_from_atom(Code,Input,Output).
	
/*
RUN_FROM_ATOM
*/
run_from_atom(Code) :-
	run_from_atom(Code,_,_).
run_from_atom(Code,Input) :-
	run_from_atom(Code,Input,_).
run_from_atom(Code,Input,Output) :-
	parse(Code,'compiled_brachylog.pl').
	
	
/*
READ_FILE
*/
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