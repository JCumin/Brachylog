package brachylog;

public class BrachylogMathPredicates {

    public static String pmCos() {
        String s = "\n"
                + Constants.PM_COS + "(X,Y) :-\n"
                + "    Y is cos(X).\n";

        return s;
    }

    public static String pmSin() {
        String s = "\n"
                + Constants.PM_SIN + "(X,Y) :-\n"
                + "    Y is sin(X).\n";

        return s;
    }

    public static String pmTan() {
        String s = "\n"
                + Constants.PM_TAN + "(X,Y) :-\n"
                + "    Y is tan(X).\n";

        return s;
    }

    public static String pmExp() {
        String s = "\n"
                + Constants.PM_EXP + "(X,Y) :-\n"
                + "    Y is exp(X).\n";

        return s;
    }

    public static String pmLog() {
        String s = "\n"
                + Constants.PM_LOG + "(X,Y) :-\n"
                + "    Y is log(X).\n";

        return s;
    }
    
    public static String pmPrimeDecomposition() {
    	String s = "\n"
    			+ "% --- CREDITS TO ROSETTA CODE ---\n"
    			+ Constants.PM_PRIMEDECOMPOSITION + "(X,Y) :-\n"
    			+ "    SN is sqrt(X),\n"
    			+ "    " + Constants.PM_PRIMEDECOMPOSITION + "_1(X, SN, 2, [], Y).\n"
    			+ Constants.PM_PRIMEDECOMPOSITION + "_1(1, _, _, L, L) :- !.\n"
    			+ Constants.PM_PRIMEDECOMPOSITION + "_1(N, SN, D, L, LF) :-\n"
    			+ "    (   0 is N mod D ->\n"
    			+ "        Q is N / D,\n"
    			+ "        SQ is sqrt(Q),\n"
    			+ "        " + Constants.PM_PRIMEDECOMPOSITION + "_1(Q, SQ, D, [D |L], LF)\n"
    			+ "    ;\n"
    			+ "        D1 is D+1,\n"
    			+ "        (    D1 > SN ->\n"
    			+ "             LF = [N |L]\n"
    			+ "        ;\n"
    			+ "             " + Constants.PM_PRIMEDECOMPOSITION + "_2(N, SN, D1, L, LF)\n"
    			+ "        )\n"
    			+ "    ).\n"
    			+ Constants.PM_PRIMEDECOMPOSITION + "_2(1, _, _, L, L) :- !.\n"
    			+ Constants.PM_PRIMEDECOMPOSITION + "_2(N, SN, D, L, LF) :-\n"
    			+ "    (   0 is N mod D ->\n"
    			+ "        Q is N / D,\n"
    			+ "        SQ is sqrt(Q),\n"
    			+ "        " + Constants.PM_PRIMEDECOMPOSITION + "_2(Q, SQ, D, [D |L], LF);\n"
    			+ "        D1 is D+2,\n"
    			+ "        (	D1 > SN ->\n"
    			+ "             LF = [N |L]\n"
    			+ "        ;\n"
    			+ "             " + Constants.PM_PRIMEDECOMPOSITION + "_2(N, SN, D1, L, LF)\n"
    			+ "        )\n"
    			+ "    ).\n";
    			
    	return s;
    }
    
    public static String pmRoot() {
        String s = "\n"
                + Constants.PM_ROOT + "(X,Y) :-\n"
                + "    Y is sqrt(X).\n";

        return s;
    }

    public static String pmFactorial() {
        String s = "\n"
                + Constants.PM_FACTORIAL + "(0,1) :- !.\n"
                + Constants.PM_FACTORIAL + "(X,Y) :-\n"
                + "    X > 0,\n"
                + "    A is X-1,\n"
                + "    " + Constants.PM_FACTORIAL + "(A,Z),\n"
                + "    Y is X*Z.\n";

        return s;
    }

    public static String pmCeil() {
        String s = "\n"
                + Constants.PM_CEIL + "(X,Y) :-\n"
                + "    Y is ceil(X).\n";

        return s;
    }

    public static String pmFloor() {
        String s = "\n"
                + Constants.PM_FLOOR + "(X,Y) :-\n"
                + "    Y is floor(X).\n";

        return s;
    }
    
    public static String pmArcCos() {
        String s = "\n"
                + Constants.PM_ARCCOS + "(X,Y) :-\n"
                + "    Y is acos(X).\n";

        return s;
    }

    public static String pmArcSin() {
        String s = "\n"
                + Constants.PM_ARCSIN + "(X,Y) :-\n"
                + "    Y is asin(X).\n";

        return s;
    }

    public static String pmArcTan() {
        String s = "\n"
                + Constants.PM_ARCTAN + "(X,Y) :-\n"
                + "    Y is atan(X).\n";

        return s;
    }
    
    public static String pmCosh() {
        String s = "\n"
                + Constants.PM_COSH + "(X,Y) :-\n"
                + "    Y is cosh(X).\n";

        return s;
    }

    public static String pmSinh() {
        String s = "\n"
                + Constants.PM_SINH + "(X,Y) :-\n"
                + "    Y is sinh(X).\n";

        return s;
    }

    public static String pmTanh() {
        String s = "\n"
                + Constants.PM_TANH + "(X,Y) :-\n"
                + "    Y is tanh(X).\n";

        return s;
    }
    
    public static String pmArcCosh() {
        String s = "\n"
                + Constants.PM_ARCCOSH + "(X,Y) :-\n"
                + "    Y is cos(X).\n";

        return s;
    }

    public static String pmArcSinh() {
        String s = "\n"
                + Constants.PM_ARCSINH + "(X,Y) :-\n"
                + "    Y is sin(X).\n";

        return s;
    }

    public static String pmArcTanh() {
        String s = "\n"
                + Constants.PM_ARCTANH + "(X,Y) :-\n"
                + "    Y is tan(X).\n";

        return s;
    }
    
    public static String pmTranspose() {
    	String s = "\n"
    			+ Constants.PM_TRANSPOSE + "(X,Y) :-\n"
    			+ "    transpose(X,Y).";
    	
    	return s;
    }
    
    public static String pmAntiTranspose() {
    	String s = "\n"
    			+ Constants.PM_ANTITRANSPOSE + "(X,Y) :-\n"
    			+ "    maplist(reverse,X,A),\n"
    			+ "    transpose(A,B),\n"
    			+ "    maplist(reverse,B,Y).\n";
    	
    	return s;
    }
}
