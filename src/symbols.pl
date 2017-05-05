/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
____            ____
\   \          /   /
 \   \  ____  /   /
  \   \/    \/   /
   \     /\     /     BRACHYLOG       
    \   /  \   /      A terse declarative logic programming language
    /   \  /   \    
   /     \/     \     Written by Julien Cumin - 2017
  /   /\____/\   \    https://github.com/JCumin/Brachylog
 /   /  ___   \   \
/___/  /__/    \___\
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- module(symbols, [token_predicate/2,
                    token_variable/2,
                    token_metapredicate/2
                   ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TOKEN_PREDICATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
token_predicate('≤', 'brachylog_lessequal').
token_predicate('≥', 'brachylog_greaterequal').
token_predicate('∈', 'brachylog_contains').
token_predicate('∋', 'brachylog_in').
token_predicate('⊆', 'brachylog_superset').
token_predicate('⊇', 'brachylog_subset').
token_predicate('↔', 'brachylog_reverse').
token_predicate('↕', '').
token_predicate('↑', '').
token_predicate('↓', '').
token_predicate('↰', 'brachylog_call_predicate').
token_predicate('↺', 'brachylog_circular_permute_counterclockwise').
token_predicate('↻', 'brachylog_circular_permute_clockwise').
token_predicate('√', 'brachylog_root').
token_predicate('⌉', 'brachylog_ceil').
token_predicate('⌋', 'brachylog_floor').
token_predicate('⟦', 'brachylog_range_ascending').
token_predicate('⟧', 'brachylog_range_descending').
token_predicate('ℕ', 'brachylog_natural_integer').
token_predicate('ℤ', 'brachylog_integer').
token_predicate('ℝ', 'brachylog_float').
token_predicate('∅', 'brachylog_empty').
token_predicate('≠', 'brachylog_different').
token_predicate('≡', '').
token_predicate('÷', 'brachylog_integer_division').
token_predicate('×', 'brachylog_multiply').
token_predicate('%', 'brachylog_modulo').
token_predicate('&', '').
token_predicate('*', 'brachylog_exp').
token_predicate('+', 'brachylog_plus').
token_predicate('-', 'brachylog_minus').
token_predicate('/', 'brachylog_divide').
token_predicate('<', 'brachylog_less').
token_predicate('=', 'brachylog_equal').
token_predicate('>', 'brachylog_greater').
token_predicate('\\', 'brachylog_transpose').
token_predicate('^', 'brachylog_power').
token_predicate('≜', 'brachylog_label').

token_predicate('a', 'brachylog_adfix').
token_predicate('b', 'brachylog_behead').
token_predicate('c', 'brachylog_concatenate').
token_predicate('d', 'brachylog_duplicates').
token_predicate('e', '').
token_predicate('f', 'brachylog_factors').
token_predicate('g', 'brachylog_group').
token_predicate('h', 'brachylog_head').
token_predicate('i', '').
token_predicate('j', 'brachylog_juxtapose').
token_predicate('k', 'brachylog_knife').
token_predicate('l', 'brachylog_length').
token_predicate('m', '').
token_predicate('n', '').
token_predicate('o', 'brachylog_order').
token_predicate('p', 'brachylog_permute').
token_predicate('q', '').
token_predicate('r', '').
token_predicate('s', 'brachylog_substring').
token_predicate('t', 'brachylog_tail').
token_predicate('u', '').
token_predicate('v', '').
token_predicate('w', 'brachylog_write').
token_predicate('x', 'brachylog_xterminate').
token_predicate('y', '').
token_predicate('z', 'brachylog_zip').

token_predicate('ạ', 'brachylog_to_codes').
token_predicate('ḅ', 'brachylog_blocks').
token_predicate('ḍ', 'brachylog_dichotomize').
token_predicate('ẹ', 'brachylog_elements').
token_predicate('ḥ', '').
token_predicate('ị', 'brachylog_to_number').
token_predicate('ḳ', '').
token_predicate('ḷ', 'brachylog_lowercase').
token_predicate('ṃ', '').
token_predicate('ṇ', 'brachylog_split_lines').
token_predicate('ọ', 'brachylog_occurences').
token_predicate('ṛ', 'brachylog_random_element').
token_predicate('ṣ', 'brachylog_shuffle').
token_predicate('ṭ', '').
token_predicate('ụ', 'brachylog_uppercase').
token_predicate('ṿ', '').
token_predicate('ẉ', 'brachylog_writeln').
token_predicate('ỵ', '').
token_predicate('ẓ', '').

token_predicate('ȧ', 'brachylog_absolute_value').
token_predicate('ḃ', 'brachylog_base').
token_predicate('ċ', 'brachylog_coerce').
token_predicate('ḋ', 'brachylog_prime_decomposition').
token_predicate('ė', '').
token_predicate('ḟ', 'brachylog_factorial').
token_predicate('ġ', '').
token_predicate('ḣ', '').
token_predicate('ṁ', '').
token_predicate('ṅ', 'brachylog_negate').
token_predicate('ȯ', '').
token_predicate('ṗ', 'brachylog_prime').
token_predicate('ṙ', 'brachylog_random_number').
token_predicate('ṡ', 'brachylog_to_string').
token_predicate('ṫ', '').
token_predicate('ẇ', '').
token_predicate('ẋ', '').
token_predicate('ẏ', '').
token_predicate('ż', '').


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TOKEN_VARIABLE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
token_variable('Ạ', '\'string\':[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]').
token_variable('Ḅ', '\'string\':[b,c,d,f,g,h,j,k,l,m,n,p,q,r,s,t,v,w,x,y,z]').
%token_variable('', '').
token_variable('Ḍ', '\'string\':[b,c,d,f,g,h,j,k,l,m,n,p,q,r,s,t,v,w,x,z]').
token_variable('Ẹ', '').
%token_variable('', '').
%token_variable('', '').
token_variable('Ḥ', '\'string\':[\'H\',e,l,l,o,\',\',\' \',\'W\',o,r,l,d,\'!\']').
token_variable('Ị', '\'string\':[\'0\',\'1\',\'2\',\'3\',\'4\',\'5\',\'6\',\'7\',\'8\',\'9\']').
%token_variable('', '').
token_variable('Ḳ', '').
token_variable('Ḷ', '\'string\':[\'\n\']').
token_variable('Ṃ', '').
token_variable('Ṇ', '\'string\':[\'A\',\'B\',\'C\',\'D\',\'E\',\'F\',\'G\',\'H\',\'I\',\'J\',\'K\',\'L\',\'M\',\'N\',\'O\',\'P\',\'Q\',\'R\',\'S\',\'T\',\'U\',\'V\',\'W\',\'X\',\'Y\',\'Z\',a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,0,1,2,3,4,5,6,7,8,9]').
token_variable('Ọ', '').
%token_variable('', '').
%token_variable('', '').
token_variable('Ṛ', '').
token_variable('Ṣ', '\'string\':[\' \']').
token_variable('Ṭ', '\'string\':[\' \',\'!\',\'"\',\'#\',\'$\',\'%\',\'&\',\'\\\'\',\'(\',\')\',\'*\',\'+\',\',\',\'-\',\'.\',\'/\',\'0\',\'1\',\'2\',\'3\',\'4\',\'5\',\'6\',\'7\',\'8\',\'9\',\':\',\';\',\'<\',\'=\',\'>\',\'?\',\'@\',\'A\',\'B\',\'C\',\'D\',\'E\',\'F\',\'G\',\'H\',\'I\',\'J\',\'K\',\'L\',\'M\',\'N\',\'O\',\'P\',\'Q\',\'R\',\'S\',\'T\',\'U\',\'V\',\'W\',\'X\',\'Y\',\'Z\',\'[\',\'\\\\\',\']\',\'^\',\'_\',\'`\',\'a\',\'b\',\'c\',\'d\',\'e\',\'f\',\'g\',\'h\',\'i\',\'j\',\'k\',\'l\',\'m\',\'n\',\'o\',\'p\',\'q\',\'r\',\'s\',\'t\',\'u\',\'v\',\'w\',\'x\',\'y\',\'z\',\'{\',\'|\',\'}\',\'~\']').
token_variable('Ụ', '').
token_variable('Ṿ', '\'string\':[a,e,i,o,u]').
token_variable('Ẉ', '\'string\':[a,e,i,o,u,y]').
%token_variable('', '').
token_variable('Ỵ', '').
token_variable('Ẓ', '\'string\':[z,y,x,w,v,u,t,s,r,q,p,o,n,m,l,k,j,i,h,g,f,e,d,c,b,a]').

token_variable('Ȧ', 'ConstraintA').
token_variable('Ḃ', 'ConstraintB').
token_variable('Ċ', 'ConstraintC').
token_variable('Ḋ', 'ConstraintD').
token_variable('Ė', 'ConstraintE').
token_variable('Ḟ', 'ConstraintF').
token_variable('Ġ', 'ConstraintG').
token_variable('Ḣ', 'ConstraintH').
token_variable('İ', 'ConstraintI').
%token_variable('', 'ConstraintJ').
%token_variable('', 'ConstraintK').
%token_variable('', 'ConstraintL').
token_variable('Ṁ', 'ConstraintM').
token_variable('Ṅ', 'ConstraintN').
token_variable('Ȯ', 'ConstraintO').
token_variable('Ṗ', 'ConstraintP').
%token_variable('', 'ConstraintQ').
token_variable('Ṙ', 'ConstraintR').
token_variable('Ṡ', 'ConstraintS').
token_variable('Ṫ', 'ConstraintT').
%token_variable('', 'ConstraintU').
%token_variable('', 'ConstraintV').
token_variable('Ẇ', 'ConstraintW').
token_variable('Ẋ', 'ConstraintX').
token_variable('Ẏ', 'ConstraintY').
token_variable('Ż', 'ConstraintZ').

token_variable('π', '\'float\':3.14159265359').
token_variable('φ', '\'float\':1.61803398875').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TOKEN_METAPREDICATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
token_metapredicate('ᵃ', 'brachylog_meta_accumulate').
token_metapredicate('ᵇ', '').
token_metapredicate('ᶜ', 'brachylog_meta_count').
token_metapredicate('ᵈ', '').
token_metapredicate('ᵉ', '').
token_metapredicate('ᶠ', 'brachylog_meta_find').
token_metapredicate('ᵍ', 'brachylog_meta_groupby').
token_metapredicate('ʰ', '').
token_metapredicate('ⁱ', 'brachylog_meta_iterate').
token_metapredicate('ʲ', '').
token_metapredicate('ᵏ', '').
token_metapredicate('ˡ', 'brachylog_meta_leftfold').
token_metapredicate('ᵐ', 'brachylog_meta_map').
token_metapredicate('ⁿ', '').
token_metapredicate('ᵒ', 'brachylog_meta_orderby').
token_metapredicate('ᵖ', '').
token_metapredicate('ʳ', 'brachylog_meta_rightfold').
token_metapredicate('ˢ', 'brachylog_meta_select').
token_metapredicate('ᵗ', '').
token_metapredicate('ᵘ', 'brachylog_meta_unique').
token_metapredicate('ᵛ', '').
token_metapredicate('ʷ', '').
token_metapredicate('ˣ', '').
token_metapredicate('ʸ', '').
token_metapredicate('ᶻ', '').
