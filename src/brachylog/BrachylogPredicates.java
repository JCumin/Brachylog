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
				+ "    number_codes(Y,T)\n"
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
				+ "    " + Constants.P_CONCATENATE + "_recur(T,S,Y).";
		
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
	
}
