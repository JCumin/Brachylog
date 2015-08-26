package brachylog;

public final class BrachylogPredicates {

	public static String pReverse() {
		
		String s = "\n"
				+ Constants.P_REVERSE + "(X,Y) :-\n"
				+ "    string(X),!,\n"
				+ "    string_codes(X,C),\n"
				+ "    reverse(C,D),\n"
				+ "    string_codes(Y,D)\n"
				+ "    ;\n"
				+ "    reverse(X,Y).\n";
		
		return s;
	}
	
	public static String pBehead() {
		
		String s = "\n"
				+ Constants.P_BEHEAD + "(X,Y) :-\n"
				+ "    string(X),!,\n"
				+ "    sub_string(X, 1, _, 0, Y)\n"
				+ "    ;\n"
				+ "    X = [_|Y].\n";
		
		return s;
	}
	
}
