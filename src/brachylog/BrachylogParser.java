package brachylog;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

public class BrachylogParser {

	public static void parseFromFile(String fileName, String input, String output) {
		
		StringBuilder program = new StringBuilder();
		BufferedReader reader;
		int c;
		try {
			File file = new File(fileName);
			reader = new BufferedReader(new InputStreamReader(new FileInputStream(file),Charset.forName("UTF-8")));
			while((c = reader.read()) != -1) {
				char character = (char) c;
				program.append(character);
			}
			reader.close();
		} catch (FileNotFoundException e) {
			System.err.println("File \"" + fileName + "\" not found.");
			return;
		} catch (IOException e) {
			e.printStackTrace();
		}
		parseFromString(program.toString(), input, output);
	}
	
	
	
	public static void parseFromString(String program, String input, String output) {
		String compiledProgram = compile(program.toString());
		run(compiledProgram, input, output);
	}
	
	
	
	private static String compile(String program) {
		
		if(program == null || program.equals("")) {
			System.err.println("Empty program. Nothing to Compile");
			return null;
		}
		
		StringBuilder prologProgram = new StringBuilder();
		
		prologProgram.append(Constants.P_MAIN + "(" + Constants.V_INPUT + "," + Constants.V_OUTPUT + ") :-\n");

		prologProgram.append("    1=1");
		
		String currentVariable = Constants.V_INPUT;
		int variableCounter = 0;
		Map<String, String> predicatesUsed = new HashMap<String, String>();
		boolean escapeNextCharacter = false;
		StringBuilder currentString = new StringBuilder();
		
		for(char c : program.toCharArray()) {
			
			//READING STRING
			if(currentString.length() > 0) {
				if(c == '\\') {
					escapeNextCharacter = true;
					currentString.append("\\");
				} else if(c == '"') {
					if(!escapeNextCharacter) {
						currentString.append("\"");
						prologProgram.append(",\n    " + currentString.toString() + " = " + currentVariable);
						currentString.setLength(0);
					} else {
						currentString.append("\"");
						escapeNextCharacter = false;
					}
				} else {
					currentString.append(c);
				}
			} 
			
			
			else {
				//VARIABLE NAME
				if(Character.isUpperCase(c)) {
					if(currentVariable.isEmpty()) {
						currentVariable = String.valueOf(c);
					} else {
						prologProgram.append(",\n    " + c + " = " + currentVariable);
					}
				} 
				
				//BEHEAD
				else if(c == 'b') {
					predicatesUsed.put("b", BrachylogPredicates.pBehead());
					prologProgram.append(",\n    " + Constants.P_BEHEAD + "(" + currentVariable + ", V" + variableCounter + ")");
					currentVariable = "V" + variableCounter++;
				} 
				
				//REVERSE
				else if(c == 'r') {
					predicatesUsed.put("r", BrachylogPredicates.pReverse());
					prologProgram.append(",\n    " + Constants.P_REVERSE + "(" + currentVariable + ", V" + variableCounter + ")");
					currentVariable = "V" + variableCounter++;
				} 
				
				//START STRING
				else if(c == '"') {
					currentString.append("\"");
				}
				
				//INPUT VARIABLE
				else if(c == '?') {
					prologProgram.append(",\n    " + Constants.V_INPUT + " = " + currentVariable);
				}
				
				//OUTPUT VARIABLE
				else if(c == '.') {
					prologProgram.append(",\n    " + Constants.V_OUTPUT + " = " + currentVariable);	
				}
				
				//AND
				else if(c == '&') {
					currentVariable = "";
				}

			}
			
		}
		prologProgram.append(".\n");
		
		for(Entry<String, String> e : predicatesUsed.entrySet()) {
			prologProgram.append(e.getValue());
		}
		
		return prologProgram.toString();
	}
	
	
	private static void run(String program, String input, String output) {
		if(program != null && !program.equals("")) {
			System.out.println(program);
			try {
				savePrologToFile(program);
			} catch(Exception e) {
				System.err.println("Failed to save temporary Prolog file.");
				return;
			}
		}
	}
	
	private static void savePrologToFile(String program) throws Exception {
		PrintWriter writer = new PrintWriter(Constants.PROLOG_FILE);
		writer.print(program);
		writer.close();
	}
	
}
