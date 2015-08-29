package Main;

import brachylog.BrachylogParser;

/**
 * BRACHYLOG
 * 
 * @author Julien Cumin
 *
 */
public class Main {
	
    public static void main(String[] args) {
    	
    	String p = "r.";
    	String i = "[1,2,3]";
    	String o = "Z";
    	
    	BrachylogParser.parseFromString(p,i,o);
    }
    
}