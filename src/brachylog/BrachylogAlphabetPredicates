package brachylog;

public abstract class BrachylogAlphabetPredicates {


    public static String paLowercase() {
        String s = "\n"
                + Constants.PA_LOWERCASE + "(X,Y) :-\n"
                + "    string(X),!,\n"
                + "    string_lower(X,Y)\n"
                + "    ;\n"
                + "    is_list(X),!,\n"
                + "    string_codes(X,A),\n"
                + "    string_lower(A,B),\n"
                + "    string_codes(Y,B)\n"
                + "    ;\n"
                + "    X = Y.\n";

        return s;
    }


    public static String paUppercase() {
        String s = "\n"
                + Constants.PA_UPPERCASE + "(X,Y) :-\n"
                + "    string(X),!,\n"
                + "    string_upper(X,Y)\n"
                + "    ;\n"
                + "    is_list(X),!,\n"
                + "    string_codes(X,A),\n"
                + "    string_upper(A,B),\n"
                + "    string_codes(Y,B)\n"
                + "    ;\n"
                + "    X = Y.\n";

        return s;
    }
}
