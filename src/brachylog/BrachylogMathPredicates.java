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
}
