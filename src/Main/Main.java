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
    	
    	//String p = "'writef(\"%s\",[\"test\"]),flush_output'";
    	String p = ",:[1:[2:5]]:3r.";
    	String i = ":[1:[2:5]]:3";
    	String o = "Z";
    	
    	BrachylogParser.parseFromString(p,i,o);
    }
    
}