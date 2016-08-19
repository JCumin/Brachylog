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


:- module(symbols, [token_predicate/2,
                    token_variable/2,
                    is_modifier/1,
                    is_variable/1,
                    is_predicate/1,
                    is_digit/1,
                    is_control_flow/1
                   ]).
              
              
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TOKEN_PREDICATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */         
token_predicate('a','brachylog_apply').
token_predicate('b','brachylog_behead').
token_predicate('c','brachylog_concatenate').
token_predicate('d','brachylog_duplicates').
token_predicate('e','brachylog_enumerate').
token_predicate('f','brachylog_findall').
token_predicate('g','brachylog_group').
token_predicate('h','brachylog_head').
token_predicate('i','brachylog_iterate').
token_predicate('j','brachylog_juxtapose').
token_predicate('k','').
token_predicate('l','brachylog_length').
token_predicate('m','brachylog_member').
token_predicate('n','').
token_predicate('o','brachylog_order').
token_predicate('p','brachylog_permute').
token_predicate('q','').
token_predicate('r','brachylog_reverse').
token_predicate('s','brachylog_subset').
token_predicate('t','brachylog_tail').
token_predicate('u','').
token_predicate('v','brachylog_void').
token_predicate('w','brachylog_write').
token_predicate('x','brachylog_xterminate').
token_predicate('y','brachylog_yield').
token_predicate('z','brachylog_zip').
token_predicate('&','brachylog_call_predicate').
token_predicate('=','brachylog_equals').
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
token_predicate('@b','brachylog_string_blocks').
token_predicate('@c','').
token_predicate('@d','').
token_predicate('@e','brachylog_string_elements').
token_predicate('@f','').
token_predicate('@g','').
token_predicate('@h','').
token_predicate('@i','').
token_predicate('@j','').
token_predicate('@k','').
token_predicate('@l','brachylog_string_lowercase').
token_predicate('@m','').
token_predicate('@n','brachylog_string_split_lines').
token_predicate('@o','').
token_predicate('@p','brachylog_string_pad').
token_predicate('@q','').
token_predicate('@r','').
token_predicate('@s','').
token_predicate('@t','').
token_predicate('@u','brachylog_string_uppercase').
token_predicate('@v','').
token_predicate('@w','brachylog_string_writeln').
token_predicate('@x','').
token_predicate('@y','').
token_predicate('@z','').
token_predicate('@0','').
token_predicate('@1','').
token_predicate('@2','brachylog_string_dichotomize').
token_predicate('@3','brachylog_string_trichotomize').
token_predicate('@4','brachylog_string_tetrachotomize').
token_predicate('@5','brachylog_string_pentachotomize').
token_predicate('@6','brachylog_string_hexachotomize').
token_predicate('@7','brachylog_string_heptachotomize').
token_predicate('@8','brachylog_string_octachotomize').
token_predicate('@9','brachylog_string_enneachotomize').
token_predicate('@+','').
token_predicate('@-','').
token_predicate('@*','').
token_predicate('@/','').
token_predicate('@\\','').
token_predicate('@^','').
token_predicate('@%','').
token_predicate('@=','').
token_predicate('@<','').
token_predicate('@>','').
token_predicate('@(','').
token_predicate('@)','').
token_predicate('@[','').
token_predicate('@]','').
token_predicate('@{','').
token_predicate('@}','').
token_predicate('@,','').
token_predicate('@;','').
token_predicate('@:','').
token_predicate('@?','brachylog_string_random_element').
token_predicate('@.','').
token_predicate('@!','').
token_predicate('@|','').
token_predicate('@_','').
token_predicate('@&','').
token_predicate('@~','brachylog_string_shuffle').
token_predicate('@"','').
token_predicate('@\'','').
token_predicate('@`','').
token_predicate('@$','brachylog_string_to_number').
token_predicate('@@','').
token_predicate('@#','').

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
token_predicate('$l','brachylog_math_ln').
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
token_predicate('$+','').
token_predicate('$-','').
token_predicate('$*','').
token_predicate('$/','brachylog_math_antitranspose').
token_predicate('$\\','brachylog_math_transpose').
token_predicate('$^','').
token_predicate('$%','').
token_predicate('$=','').
token_predicate('$<','').
token_predicate('$>','').
token_predicate('$(','brachylog_math_circular_permute_left').
token_predicate('$)','brachylog_math_circular_permute_right').
token_predicate('$[','brachylog_math_floor').
token_predicate('$]','brachylog_math_ceil').
token_predicate('${','').
token_predicate('$}','').
token_predicate('$,','').
token_predicate('$;','').
token_predicate('$:','').
token_predicate('$?','brachylog_math_random_number').
token_predicate('$.','').
token_predicate('$!','brachylog_math_factorial').
token_predicate('$|','brachylog_math_norm').
token_predicate('$_','brachylog_math_negate').
token_predicate('$&','').
token_predicate('$~','').
token_predicate('$"','').
token_predicate('$\'','').
token_predicate('$`','').
token_predicate('$$','').
token_predicate('$@','brachylog_math_to_string').
token_predicate('$#','').

token_predicate('#a','').
token_predicate('#b','').
token_predicate('#c','').
token_predicate('#d','brachylog_constraint_different').
token_predicate('#e','').
token_predicate('#f','').
token_predicate('#g','').
token_predicate('#h','').
token_predicate('#i','').
token_predicate('#j','').
token_predicate('#k','').
token_predicate('#l','').
token_predicate('#m','').
token_predicate('#n','').
token_predicate('#o','').
token_predicate('#p','').
token_predicate('#q','').
token_predicate('#r','').
token_predicate('#s','').
token_predicate('#t','').
token_predicate('#u','').
token_predicate('#v','').
token_predicate('#w','').
token_predicate('#x','').
token_predicate('#y','').
token_predicate('#z','').
token_predicate('#0','brachylog_constraint_digit').
token_predicate('#1','brachylog_constraint_nonzero_digit').
token_predicate('#2','').
token_predicate('#3','').
token_predicate('#4','').
token_predicate('#5','').
token_predicate('#6','').
token_predicate('#7','').
token_predicate('#8','').
token_predicate('#9','').
token_predicate('#+','brachylog_constraint_positive').
token_predicate('#-','').
token_predicate('#*','').
token_predicate('#/','').
token_predicate('#\\','').
token_predicate('#^','').
token_predicate('#%','').
token_predicate('#=','brachylog_constraint_all_equal').
token_predicate('#<','brachylog_constraint_strictly_negative').
token_predicate('#>','brachylog_constraint_strictly_positive').
token_predicate('#(','').
token_predicate('#)','').
token_predicate('#[','').
token_predicate('#]','').
token_predicate('#{','').
token_predicate('#}','').
token_predicate('#,','').
token_predicate('#;','').
token_predicate('#:','').
token_predicate('#?','').
token_predicate('#.','').
token_predicate('#!','').
token_predicate('#|','').
token_predicate('#_','brachylog_constraint_negative').
token_predicate('#&','').
token_predicate('#~','').
token_predicate('#"','').
token_predicate('#\'','').
token_predicate('#`','').
token_predicate('#$','brachylog_constraint_coerce_to_integer').
token_predicate('#@','brachylog_constraint_coerce_to_string').
token_predicate('##','brachylog_constraint_coerce_to_list').


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TOKEN_VARIABLE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
token_variable('@':'A','\'string\':[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]').
token_variable('@':'B','').
token_variable('@':'C','\'string\':[b,c,d,f,g,h,j,k,l,m,n,p,q,r,s,t,v,w,x,y,z]').
token_variable('@':'D','\'string\':[b,c,d,f,g,h,j,k,l,m,n,p,q,r,s,t,v,w,x,z]').
token_variable('@':'E','').
token_variable('@':'F','').
token_variable('@':'G','').
token_variable('@':'H','\'string\':[\'H\',e,l,l,o,\',\',\' \',\'W\',o,r,l,d,\'!\']').
token_variable('@':'I','\'string\':[0,1,2,3,4,5,6,7,8,9]').
token_variable('@':'J','').
token_variable('@':'K','').
token_variable('@':'L','').
token_variable('@':'M','').
token_variable('@':'N','\'string\':[\'\n\']').
token_variable('@':'O','').
token_variable('@':'P','\'string\':[\' \',\'!\',\'"\',\'#\',\'$\',\'%\',\'&\',\'\\\'\',\'(\',\')\',\'*\',\'+\',\',\',\'-\',\'.\',\'/\',\'0\',\'1\',\'2\',\'3\',\'4\',\'5\',\'6\',\'7\',\'8\',\'9\',\':\',\';\',\'<\',\'=\',\'>\',\'?\',\'@\',\'A\',\'B\',\'C\',\'D\',\'E\',\'F\',\'G\',\'H\',\'I\',\'J\',\'K\',\'L\',\'M\',\'N\',\'O\',\'P\',\'Q\',\'R\',\'S\',\'T\',\'U\',\'V\',\'W\',\'X\',\'Y\',\'Z\',\'[\',\'\\\\\',\']\',\'^\',\'_\',\'`\',\'a\',\'b\',\'c\',\'d\',\'e\',\'f\',\'g\',\'h\',\'i\',\'j\',\'k\',\'l\',\'m\',\'n\',\'o\',\'p\',\'q\',\'r\',\'s\',\'t\',\'u\',\'v\',\'w\',\'x\',\'y\',\'z\',\'{\',\'|\',\'}\',\'~\']').
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

token_variable('$':'A','\'float\':1.61803398875').
token_variable('$':'B','\'float\':0.69314718056').
token_variable('$':'C','').
token_variable('$':'D','\'float\':2.30258509299').
token_variable('$':'E','\'float\':2.71828182846').
token_variable('$':'F','').
token_variable('$':'G','\'integer\':1000000000').
token_variable('$':'H','').
token_variable('$':'I','\'integer\':infinite').
token_variable('$':'J','').
token_variable('$':'K','\'integer\':1000').
token_variable('$':'L','').
token_variable('$':'M','\'integer\':1000000').
token_variable('$':'N','').
token_variable('$':'O','').
token_variable('$':'P','\'float\':3.14159265359').
token_variable('$':'Q','').
token_variable('$':'R','\'float\':1.41421356237').
token_variable('$':'S','').
token_variable('$':'T','').
token_variable('$':'U','').
token_variable('$':'V','').
token_variable('$':'W','').
token_variable('$':'X','').
token_variable('$':'Y','').
token_variable('$':'Z','').

token_variable('#':'A','').
token_variable('#':'B','').
token_variable('#':'C','').
token_variable('#':'D','').
token_variable('#':'E','').
token_variable('#':'F','').
token_variable('#':'G','').
token_variable('#':'H','').
token_variable('#':'I','').
token_variable('#':'J','').
token_variable('#':'K','').
token_variable('#':'L','').
token_variable('#':'M','').
token_variable('#':'N','').
token_variable('#':'O','').
token_variable('#':'P','').
token_variable('#':'Q','').
token_variable('#':'R','').
token_variable('#':'S','').
token_variable('#':'T','').
token_variable('#':'U','').
token_variable('#':'V','').
token_variable('#':'W','').
token_variable('#':'X','').
token_variable('#':'Y','').
token_variable('#':'Z','').


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   IS_MODIFIER
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
is_modifier(X) :-
    X = '@'
    ;
    X = '$'
    ;
    X = '#'.

    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   IS_VARIABLE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
is_variable(X) :-
    member(X, ['A', 'B', 'C', 'D', 'E', 'F',
               'G', 'H', 'I', 'J', 'K', 'L',
               'M', 'N', 'O', 'P', 'Q', 'R',
               'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']).
        
        
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   IS_PREDICATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */  
is_predicate(X) :-
    member(X, ['a', 'b', 'c', 'd', 'e', 'f',
               'g', 'h', 'i', 'j', 'k', 'l',
               'm', 'n', 'o', 'p', 'q', 'r',
               's', 't', 'u', 'v', 'w', 'x', 'y', 'z']).
        
        
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   IS_DIGIT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */      
is_digit(X) :-
    member(X, ['0', '1', '2', '3', '4', 
               '5', '6', '7', '8', '9']).
               
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   IS_CONTROL_FLOW
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
is_control_flow(X) :-
    member(X, [',', ';', ':', '!', '\\',
               '\'', '~', '(', ')', '\n',
               '|', '{', '}', '`', ' ']).
