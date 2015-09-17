package brachylog;

public abstract class BrachylogPredicates {
	
	
	public static String pBehead() {
		
		String s = "\n"
				+ Constants.P_BEHEAD + "(X,Y) :-\n"
				+ "    string(X),!,\n"
				+ "    sub_string(X, 1, _, 0, Y)\n"
				+ "    ;\n"
				+ "    number(X),!,\n"
				+ "    number_codes(X,[_|T]),\n"
				+ "    catch(number_codes(Y,T),_,Y=[])\n"
				+ "    ;\n"
				+ "    atom(X),!,\n"
				+ "    atom_codes(X,[_|T]),\n"
				+ "    atom_codes(Y,T)\n"
				+ "    ;\n"
				+ "    X = [_|Y].\n";
		
		return s;
	}
	
	
	public static String pConcatenate() {
		
		String s = "\n"
				+ Constants.P_CONCATENATE + "([A|T],Y) :-\n"
				+ "    " + Constants.P_CONCATENATE + "_recur" + "(T,A,Y).\n"
				+ Constants.P_CONCATENATE + "_recur([],Y,Y) :- !.\n"
				+ Constants.P_CONCATENATE + "_recur([A|T],R,Y) :-\n"
				+ "    (\n"
				+ "    string(R),!,\n"
				+ "    string_concat(R,A,S)\n"
				+ "    ;\n"
				+ "    number(R),!,\n"
				+ "    number_codes(R,C),\n"
				+ "    number_codes(A,D),\n"
				+ "    append(C,D,E),\n"
				+ "    number_codes(S,E)\n"
				+ "    ;\n"
				+ "    atom(R),!,\n"
				+ "    atom_codes(R,C),\n"
				+ "    atom_codes(A,D),\n"
				+ "    append(C,D,E),\n"
				+ "    atom_codes(S,E)\n"
				+ "    ;\n"
				+ "    append(R,A,S)\n"
				+ "    ),\n"
				+ "    " + Constants.P_CONCATENATE + "_recur(T,S,Y).\n";
		
		return s;
	}
	
	public static String pDuplicates() {
		
		String s = "\n"
				+ Constants.P_DUPLICATES + "(X,Y) :-\n"
				+ "    string(X),!,\n"
				+ "    string_codes(X,C),\n"
				+ "    list_to_set(C,S),\n"
				+ "    string_codes(Y,S)\n"
				+ "    ;\n"
				+ "    number(X),!,\n"
				+ "    number_codes(X,C),\n"
				+ "    list_to_set(C,S),\n"
				+ "    number_codes(Y,S)\n"
				+ "    ;\n"
				+ "    atom(X),!,\n"
				+ "    atom_codes(X,C),\n"
				+ "    list_to_set(C,S),\n"
				+ "    atom_codes(Y,S)\n"
				+ "    ;"
				+ "    list_to_set(X,Y).\n";
		
		return s;
	}
	
	public static String pEnumerate() {
		
		String s = "\n"
				+ Constants.P_ENUMERATE + "([A,B],Y) :-\n"
				+ "    between(A,B,Y).\n"
				+ Constants.P_ENUMERATE + "(A,Y) :-\n"
				+ "    string(A),!,\n"
				+ "    string_length(A,L),\n"
				+ "    M is L-1,\n"
				+ "    between(0,M,I),\n"
				+ "    sub_string(A,I,1,_,Y).\n";
		
		return s;
	}
	
	
	public static String pFindAll() {
		String s = "\n"
				+ Constants.P_FINDALL + "(X,Y) :-\n"
				+ "    findall(A," + Constants.P_CALLPREDICATE + "([A,X],_), Y).\n";
		
		return s;
	}
	
	
	public static String pHead() {
		
		String s = "\n"
				+ Constants.P_HEAD + "(X,Y) :-\n"
				+ "    string(X),!,\n"
				+ "    sub_string(X, 0, 1, _, Y)\n"
				+ "    ;\n"
				+ "    number(X),!,\n"
				+ "    number_codes(X,[A|_]),\n"
				+ "    number_codes(Y,[A])\n"
				+ "    ;\n"
				+ "    atom(X),!,\n"
				+ "    atom_codes(X,[A|_]),\n"
				+ "    atom_codes(Y,[A])\n"
				+ "    ;\n"
				+ "    X = [Y|_].\n";
		
		return s;
	}
	
	
	public static String pLength() {
		
		String s = "\n"
				+ Constants.P_LENGTH + "(X,Y) :-\n"
				+ "    (atom(X) ; number(X) ; string(X)),!,\n"
				+ "    atom_length(X,Y)\n"
				+ "    ;\n"
				+ "    length(X,Y).\n";
		
		return s;
	}
	
	
	public static String pMember() {

        String s = "\n"
                + Constants.P_MEMBER + "(X,Y) :-\n"
                + "    X = [String,Index],\n"
                + "    string(String),!,\n"
                + "    string_codes(String,CString),\n"
                + "    nth0(Index,CString,C),\n"
                + "    string_codes(Y,[C])\n"
                + "    ;\n"
                + "    X = [String,Index,Rest],\n"
                + "    string(String),!,\n"
                + "    string_codes(String,CString),\n"
                + "    nth0(Index,CString,C,CRest),\n"
                + "    string_codes(Rest,CRest),\n"
                + "    string_codes(Y,[C])\n"
                + "    ;\n"
                + "    X = [Number,Index],\n"
                + "    number(Number),!,\n"
                + "    number_codes(Number,CNumber),\n"
                + "    nth0(Index,CNumber,C),\n"
                + "    number_codes(Y,[C])\n"
                + "    ;\n"
                + "    X = [Number,Index,Rest],\n"
                + "    number(Number),!,\n"
                + "    number_codes(Number,CNumber),\n"
                + "    nth0(Index,CNumber,C,CRest),\n"
                + "    string_codes(Rest,CRest),\n"
                + "    string_codes(Y,[C])\n"
                + "    ;\n"
                + "    X = [List,Index],!,\n"
                + "    nth0(Index,List,Y)\n"
                + "    ;\n"
                + "    X = [List,Index,Rest],!,\n"
                + "    nth0(Index,List,Y,Rest).\n";
                        
        return s;
    }
	
	
	public static String pOrder() {

		String s = "\n"
				+ Constants.P_ORDER + "(X,Y) :-\n"
				+ "    string(X),!,\n"
				+ "    string_codes(X,C),\n"
				+ "    msort(C,D),\n"
				+ "    string_codes(Y,D)\n"
				+ "    ;\n"
				+ "    number(X),!,\n"
				+ "    number_codes(X,C),\n"
				+ "    msort(C,D),\n"
				+ "    number_codes(Y,D)\n"
				+ "    ;\n"
				+ "    atom(X),!,\n"
				+ "    atom_codes(X,C),\n"
				+ "    msort(C,D),\n"
				+ "    atom_codes(Y,D)\n"
				+ "    ;\n"
				+ "    msort(X,Y).\n";

		return s;
	}
	
	
	public static String pPermute() {

		String s = "\n"
				+ Constants.P_PERMUTE + "(X,Y) :-\n"
				+ "    string(X),!,\n"
				+ "    string_codes(X,C),\n"
				+ "    permutation(C,D),\n"
				+ "    string_codes(Y,D)\n"
				+ "    ;\n"
				+ "    number(X),!,\n"
				+ "    number_codes(X,C),\n"
				+ "    permutation(C,D),\n"
				+ "    number_codes(Y,D)\n"
				+ "    ;\n"
				+ "    atom(X),!,\n"
				+ "    atom_codes(X,C),\n"
				+ "    permutation(C,D),\n"
				+ "    atom_codes(Y,D)\n"
				+ "    ;\n"
				+ "    permutation(X,Y).\n";

		return s;
	}
	
	
	public static String pReverse() {
		
		String s = "\n"
				+ Constants.P_REVERSE + "(X,Y) :-\n"
				+ "    string(X),!,\n"
				+ "    string_codes(X,C),\n"
				+ "    reverse(C,D),\n"
				+ "    string_codes(Y,D)\n"
				+ "    ;\n"
				+ "    number(X),!,\n"
				+ "    number_codes(X,E),\n"
				+ "    reverse(E,F),\n"
				+ "    number_codes(Y,F)\n"
				+ "    ;\n"
				+ "    atom(X),!,\n"
				+ "    atom_codes(X,G),\n"
				+ "    reverse(G,H),\n"
				+ "    atom_codes(Y,H)\n"
				+ "    ;\n"
				+ "    reverse(X,Y).\n";
		return s;
	}
	
	
	public static String pSubset() {
		String s = "\n"
				+ Constants.P_SUBSET + "(X,Y) :-\n"
				+ "    string(X),!,\n"
				+ "    string_codes(X,C),\n"
				+ "    brachylog_subset_recur(C,D),\n"
				+ "    string_codes(Y,D)\n"
				+ "    ;\n"
				+ "    number(X),!,\n"
				+ "    number_codes(X,C),\n"
				+ "    brachylog_subset_recur(C,D),\n"
				+ "    catch(number_codes(Y,D),_,fail)\n"
				+ "    ;\n"
				+ "    atom(X),!,\n"
				+ "    atom_codes(X,C),\n"
				+ "    brachylog_subset_recur(C,D),\n"
				+ "    atom_codes(Y,D)\n"
				+ "    ;\n"
				+ "    brachylog_subset_recur(X,Y).\n"
				+ Constants.P_SUBSET + "_recur([],[]).\n"
				+ Constants.P_SUBSET + "_recur([H|T],[H|T2]) :-\n"
				+ "    " + Constants.P_SUBSET + "_recur(T,T2).\n"
				+ Constants.P_SUBSET + "_recur([_|T],T2) :-\n"
				+ "    " + Constants.P_SUBSET + "_recur(T,T2).\n";
		
		return s;
	}
	
	
	public static String pWrite() {

        String s = "\n"
                + Constants.P_WRITE + "(X,Y) :-\n"
                + "    X = [List,Format],\n"
                + "    is_list(List),\n"
                + "    string(Format),!,\n"
                + "    format(Format,List),\n"
                + "    flush_output,\n"
                + "    Y = List\n"
                + "    ;\n"
                + "    write(X),\n"
                + "    flush_output,\n"
                + "    Y = X.\n";
        
        return s;
	}
	
	
	public static String pXterminate() {

        String s = "\n"
                + Constants.P_XTERMINATE + "(X,Y) :-\n"
                + "    X = [List,Elem],\n"
                + "    (\n"
                + "    string(List),!,\n"
                + "    string_codes(List,C),\n"
                + "    string_codes(Elem,[F|[]]),\n"
                + "    delete(C,F,D),\n"
                + "    string_codes(Y,D)\n"
                + "    ;\n"
                + "    number(List),!,\n"
                + "    number_codes(List,C),\n"
                + "    number_codes(Elem,[F|[]]),\n"
                + "    delete(C,F,D),\n"
                + "    catch(number_codes(Y,D),_,fail)\n"
                + "    ;\n"
                + "    atom(List),!,\n"
                + "    atom_codes(List,C),\n"
                + "    atom_codes(Elem,[F|[]]),\n"
                + "    delete(C,F,D),\n"
                + "    atom_codes(Y,D)\n"
                + "    ;\n"
                + "    delete(List,Elem,Y)\n"
                + "    ).\n";

        return s;
	}
	
	
	public static String pCallPredicate() {
		
		String s = "\n"
				+ Constants.P_CALLPREDICATE + "(X,Y) :-\n"
				+ "    reverse(X,R),\n"
				+ "    R = [N|RArgs],\n"
				+ "    number(N),\n"
				+ "    reverse(RArgs, Args),\n"
				+ "    (\n"
				+ "    N = 0,!,\n"
				+ "    Name = " + Constants.P_MAIN + "\n"
				+ "    ;\n"
				+ "    atom_concat(" + Constants.P_SUBPREDICATE + ",N,Name)\n"
				+ "    ),\n"
				+ "    (\n"
				+ "    Args = [UniqueArg],!,\n"
				+ "    call(Name,UniqueArg,Y)\n"
				+ "    ;\n"
				+ "    call(Name,Args,Y)\n"
				+ "    ).\n";
				
		return s;
	}
	
	
	public static String pLess() {
		
		String s = "\n"
				+ Constants.P_LESS + "([A,B],Y) :-\n"
				+ "    (string(A) ; atom(A) ; is_list(A)),!,\n"
				+ "    A @< B,\n"
				+ "    Y = A\n"
				+ "    ;\n"
				+ "    A < B,\n"
				+ "    Y = A.\n";
		
		return s;
	}
	
	public static String pGreater() {
		
		String s = "\n"
				+ Constants.P_GREATER + "([A,B],Y) :-\n"
				+ "    (string(A) ; atom(A) ; is_list(A)),!,\n"
				+ "    A @> B,\n"
				+ "    Y = A\n"
				+ "    ;\n"
				+ "    A > B,\n"
				+ "    Y = A.\n";
		
		return s;
	}
	
	public static String pLessEqual() {
		
		String s = "\n"
				+ Constants.P_LESSEQUAL + "([A,B],Y) :-\n"
				+ "    (string(A) ; atom(A) ; is_list(A)),!,\n"
				+ "    A @=< B,\n"
				+ "    Y = A\n"
				+ "    ;\n"
				+ "    A =< B,\n"
				+ "    Y = A.\n";
		
		return s;
	}
	
	public static String pGreaterEqual() {
		
		String s = "\n"
				+ Constants.P_GREATEREQUAL + "([A,B],Y) :-\n"
				+ "    (string(A) ; atom(A) ; is_list(A)),!,\n"
				+ "    A @>= B,\n"
				+ "    Y = A\n"
				+ "    ;\n"
				+ "    A >= B,\n"
				+ "    Y = A.\n";
		
		return s;
	}
}
