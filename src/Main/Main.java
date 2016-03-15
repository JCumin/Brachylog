package Main;

import brachylog.BrachylogParser;
import java.nio.file.Files;
import java.io.File;
import java.io.IOException;

/**
 * BRACHYLOG
 * 
 * @author Julien Cumin
 *
 */
public class Main {
	
    public static void main(String[] args) throws IOException {
    	

    	String p = new String(Files.readAllBytes(new File(args[0]).toPath()));
    	
    	String i = "[1:1]:[0:1]";
    	String o = "X";
    	
    	BrachylogParser.parseFromString(p,i,o);
    }
    
}
