:- module(symbols, [token_predicate/2,
					token_variable/2,
					is_modifier/1,
					is_variable/1,
					is_predicate/1,
					is_digit/1,
					is_control_flow/1]).
					
/*
TOKEN_PREDICATE
*/					
token_predicate('a','').
token_predicate('b','brachylog_behead').
token_predicate('c','brachylog_concatenate').
token_predicate('d','brachylog_duplicates').
token_predicate('e','brachylog_enumerate').
token_predicate('f','brachylog_findall').
token_predicate('g','').
token_predicate('h','brachylog_head').
token_predicate('i','').
token_predicate('j','').
token_predicate('k','').
token_predicate('l','brachylog_length').
token_predicate('m','brachylog_member').
token_predicate('n','').
token_predicate('o','brachylog_order').
token_predicate('p','brachylog_permute').
token_predicate('q','').
token_predicate('r','brachylog_reverse').
token_predicate('s','brachylog_subset').
token_predicate('t','').
token_predicate('u','').
token_predicate('v','').
token_predicate('w','brachylog_write').
token_predicate('x','brachylog_xterminate').
token_predicate('y','').
token_predicate('z','').
token_predicate('&','brachylog_call_predicate').
token_predicate('=','brachylog_equal').
token_predicate('<','brachylog_less').
token_predicate('>','brachylog_greater').
token_predicate('^','brachylog_power').
token_predicate('*','brachylog_multiply').
token_predicate('/','brachylog_divide').
token_predicate('%','brachylog_modulo').
token_predicate('+','brachylog_plus').
token_predicate('-','brachylog_minus').
token_predicate('<=','brachylog_lessequal').
token_predicate('>=','brachylog_greaterequal').

token_predicate('@a','').
token_predicate('@b','').
token_predicate('@c','').
token_predicate('@d','').
token_predicate('@e','').
token_predicate('@f','').
token_predicate('@g','').
token_predicate('@h','').
token_predicate('@i','').
token_predicate('@j','').
token_predicate('@k','').
token_predicate('@l','brachylog_string_lowercase').
token_predicate('@m','').
token_predicate('@n','').
token_predicate('@o','').
token_predicate('@p','').
token_predicate('@q','').
token_predicate('@r','').
token_predicate('@s','').
token_predicate('@t','').
token_predicate('@u','brachylog_string_uppercase').
token_predicate('@v','').
token_predicate('@w','').
token_predicate('@x','').
token_predicate('@y','').
token_predicate('@z','').
token_predicate('@&','').
token_predicate('@=','').
token_predicate('@<','').
token_predicate('@>','').
token_predicate('@^','').
token_predicate('@/','').
token_predicate('@%','').
token_predicate('@+','').
token_predicate('@-','').

token_predicate('$a','').
token_predicate('$b','').
token_predicate('$c','brachylog_math_cos').
token_predicate('$d','').
token_predicate('$e','brachylog_math_exp').
token_predicate('$f','').
token_predicate('$g','').
token_predicate('$h','').
token_predicate('$i','').
token_predicate('$j','').
token_predicate('$k','').
token_predicate('$l','brachylog_math_log').
token_predicate('$m','').
token_predicate('$n','').
token_predicate('$o','').
token_predicate('$p','brachylog_math_prime_decomposition').
token_predicate('$q','').
token_predicate('$r','brachylog_math_root').
token_predicate('$s','brachylog_math_sin').
token_predicate('$t','brachylog_math_tan').
token_predicate('$u','').
token_predicate('$v','').
token_predicate('$w','').
token_predicate('$x','').
token_predicate('$y','').
token_predicate('$z','').
token_predicate('$&','').
token_predicate('$=','').
token_predicate('$<','').
token_predicate('$>','').
token_predicate('$^','').
token_predicate('$/','').
token_predicate('$%','').
token_predicate('$+','').
token_predicate('$-','').
token_predicate('$0','').
token_predicate('$1','brachylog_math_arccos').
token_predicate('$2','brachylog_math_arcsin').
token_predicate('$3','brachylog_math_arctan').
token_predicate('$4','brachylog_math_cosh').
token_predicate('$5','brachylog_math_sinh').
token_predicate('$6','brachylog_math_tanh').
token_predicate('$7','brachylog_math_argcosh').
token_predicate('$8','brachylog_math_argsinh').
token_predicate('$9','brachylog_math_argtanh').
token_predicate('$!','brachylog_math_factorial').
token_predicate('$[','brachylog_math_floor').
token_predicate('$]','brachylog_math_ceil').
token_predicate('$\\','brachylog_math_transpose').
token_predicate('$/','brachylog_math_anti_transpose').
token_predicate('$(','brachylog_math_circular_permutation_left').
token_predicate('$)','brachylog_math_circular_permutation_right').


/*
TOKEN_VARIABLE
*/
token_variable('@':'A','\'string\':[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]').
token_variable('@':'B','\'string\':[b,c,d,f,g,h,j,k,l,m,n,p,q,r,s,t,v,w,x,y,z]').
token_variable('@':'C','\'string\':[b,c,d,f,g,h,j,k,l,m,n,p,q,r,s,t,v,w,x,z]').
token_variable('@':'D','').
token_variable('@':'E','').
token_variable('@':'F','').
token_variable('@':'G','').
token_variable('@':'H','\'string\':[\'H\',e,l,l,o,\',\',\' \',\'W\',o,r,l,d,\'!\']').
token_variable('@':'I','').
token_variable('@':'J','').
token_variable('@':'K','').
token_variable('@':'L','').
token_variable('@':'M','').
token_variable('@':'N','\'string\':[\'\n\']').
token_variable('@':'O','').
token_variable('@':'P','').
token_variable('@':'Q','\'string\':[\'@\',\'Q\',w]').
token_variable('@':'R','').
token_variable('@':'S','\'string\':[\' \']').
token_variable('@':'T','\'string\':[\'\t\']').
token_variable('@':'U','').
token_variable('@':'V','\'string\':[a,e,i,o,u]').
token_variable('@':'W','\'string\':[a,e,i,o,u,y]').
token_variable('@':'X','').
token_variable('@':'Y','').
token_variable('@':'Z','\'string\':[z,y,x,w,v,u,t,s,r,q,p,o,n,m,l,k,j,i,h,g,f,e,d,c,b,a]').

token_variable('$':'A','').
token_variable('$':'B','').
token_variable('$':'C','').
token_variable('$':'D','').
token_variable('$':'E','').
token_variable('$':'F','').
token_variable('$':'G','').
token_variable('$':'H','').
token_variable('$':'I','').
token_variable('$':'J','').
token_variable('$':'K','').
token_variable('$':'L','').
token_variable('$':'M','').
token_variable('$':'N','').
token_variable('$':'O','').
token_variable('$':'P','').
token_variable('$':'Q','').
token_variable('$':'R','').
token_variable('$':'S','').
token_variable('$':'T','').
token_variable('$':'U','').
token_variable('$':'V','').
token_variable('$':'W','').
token_variable('$':'X','').
token_variable('$':'Y','').
token_variable('$':'Z','').


/*
IS_MODIFIER
*/
is_modifier(X) :-
	X = '@'
	;
	X = '$'
	;
	X = '#'.

/*
IS_VARIABLE
*/	
is_variable(X) :-
	member(X, ['A', 'B', 'C', 'D', 'E', 'F',
               'G', 'H', 'I', 'J', 'K', 'L',
			   'M', 'N', 'O', 'P', 'Q', 'R',
			   'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']).
		
/*
IS_PREDICATE
*/		
is_predicate(X) :-
	member(X, ['a', 'b', 'c', 'd', 'e', 'f',
               'g', 'h', 'i', 'j', 'k', 'l',
			   'm', 'n', 'o', 'p', 'q', 'r',
			   's', 't', 'u', 'v', 'w', 'x', 'y', 'z']).
		
/*
IS_DIGIT
*/		
is_digit(X) :-
	member(X, ['0', '1', '2', '3', '4', 
	           '5', '6', '7', '8', '9']).
			   
/*
IS_CONTROL_FLOW
*/
is_control_flow(X) :-
	member(X, [',', ';', ':', '!', '\\',
			   '\'', '~', '(', ')', '\n', '|']).
