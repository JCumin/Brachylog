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
    	

    	String p = "~l.";
    	
    	String i = "2";
    	String o = "X";
    	
    	BrachylogParser.parseFromString(p,i,o);
    }
    
}