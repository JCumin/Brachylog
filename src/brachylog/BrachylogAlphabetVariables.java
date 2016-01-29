package brachylog;

public abstract class BrachylogAlphabetVariables {

    public static String getValueForName(char c) {
        String s;
        switch(c) {
            case 'A': s = "\"abcdefghijklmnopqrstuvwxyz\"";
                      break;
            case 'B': s = "";
                      break;
            case 'C': s = "\"bcdfghjklmnpqrstvwxyz\"";
                      break;
            case 'D': s = "\"bcdfghjklmnpqrstvwxz\"";
                      break;
            case 'E': s = "";
                      break;
            case 'F': s = "";
                      break;
            case 'G': s = "";
                      break;
            case 'H': s = "\"Hello, World!\"";
                      break;
            case 'I': s = "";
                      break;
            case 'J': s = "";
                      break;
            case 'K': s = "";
                      break;
            case 'L': s = "";
                      break;
            case 'M': s = "";
                      break;
            case 'N': s = "\"\n\"";
                      break;
            case 'O': s = "";
                      break;
            case 'P': s = "";
                      break;
            case 'Q': s = "\"@Qw\"";
                      break;
            case 'R': s = "";
                      break;
            case 'S': s = "\" \"";
                      break;
            case 'T': s = "\"\t\"";
                      break;
            case 'U': s = "";
                      break;
            case 'V': s = "\"aeiou\"";
                      break;
            case 'W': s = "\"aeiouy\"";
                      break;
            case 'X': s = "";
                      break;
            case 'Y': s = "";
                      break;
            case 'Z': s = "\"zyxwvutsrqponmlkjihgfedcba\"";
                      break;
            default:  s = null;
                      break;
        }
        return s;
    }

}
