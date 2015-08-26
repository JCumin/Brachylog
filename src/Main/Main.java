package Main;

import brachylog.BrachylogParser;

public class Main {
	
    public static void main(String[] args) {
    	
    	String p = "b.";
    	String i = "[1,2,3]";
    	String o = "Z";
    	
    	BrachylogParser.parseFromString(p,i,o);
    }
    
}