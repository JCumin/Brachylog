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
    	

    	String p = "_|h{_|(h1;?h0),?b:1&},?b:0&";
    	
    	String i = "[1:1]:[0:1]";
    	String o = "X";
    	
    	BrachylogParser.parseFromString(p,i,o);
    }
    
}