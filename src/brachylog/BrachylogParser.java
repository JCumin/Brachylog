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
import java.util.Stack;

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
		
		Stack<String> currentVariables = new Stack<String>();
		currentVariables.push(Constants.V_INPUT);
		int variableCounter = 0;
		Map<String, String> predicatesUsed = new HashMap<String, String>();
		boolean escapeNextCharacter = false;
		StringBuilder currentString = new StringBuilder();
		boolean readingNumber = false;
		
		for(char c : program.toCharArray()) {
			
			//READING STRING
			if(currentString.length() > 0) {
				if(c == '\\') {
					escapeNextCharacter = true;
					currentString.append("\\");
				} else if(c == '"') {
					if(!escapeNextCharacter) {
						currentString.append("\"");
						if(currentVariables.lastElement().isEmpty()) {
							currentVariables.pop();
							currentVariables.push(currentString.toString());
						} else {
							prologProgram.append(",\n    " + currentString.toString() + " = " + currentVariables.lastElement());
						}
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
					if(currentVariables.lastElement().isEmpty()) {
						currentVariables.pop();
						currentVariables.push(String.valueOf(c));
					} else {
						prologProgram.append(",\n    " + c + " = " + currentVariables.lastElement());
					}
				}
				
				//NUMBER
				if(Character.isDigit(c)) {
					if(!readingNumber) {
						prologProgram.append(",\n    " + currentVariables.lastElement() + " = " + c);
						readingNumber = true;
					} else {
						prologProgram.append(c);
					}
				} else {
					if(readingNumber && c == '.') {
						prologProgram.append(c);
					} else {
						readingNumber = false;
					}
				}
				
				//BEHEAD
				if(c == 'b') {
					predicatesUsed.put("b", BrachylogPredicates.pBehead());
					prologProgram.append(",\n    " + Constants.P_BEHEAD + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
					currentVariables.pop();
					currentVariables.push("V" + variableCounter++);
				} 
				
				//LENGTH
				else if(c == 'l') {
					predicatesUsed.put("l", BrachylogPredicates.pLength());
					prologProgram.append(",\n    " + Constants.P_LENGTH + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
					currentVariables.pop();
					currentVariables.push("V" + variableCounter++);
				}
				
				//REVERSE
				else if(c == 'r') {
					predicatesUsed.put("r", BrachylogPredicates.pReverse());
					prologProgram.append(",\n    " + Constants.P_REVERSE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
					currentVariables.pop();
					currentVariables.push("V" + variableCounter++);
				} 
				
				//START STRING
				else if(c == '"') {
					currentString.append("\"");
				}
				
				//
				
				//INPUT VARIABLE
				else if(c == '?') {
					if(currentVariables.lastElement().isEmpty()) {
						currentVariables.pop();
						currentVariables.push(String.valueOf(c));
					} else {
						prologProgram.append(",\n    " + Constants.V_INPUT + " = " + currentVariables.lastElement());
					}
				}
				
				//OUTPUT VARIABLE
				else if(c == '.') {
					if(!readingNumber) {
						if(currentVariables.lastElement().isEmpty()) {
							currentVariables.pop();
							currentVariables.push(String.valueOf(c));
						} else {
							prologProgram.append(",\n    " + Constants.V_OUTPUT + " = " + currentVariables.lastElement());		
						}
					}
				}
				
				//AND
				else if(c == '&') {
					currentVariables.pop();
					currentVariables.push("");
				}
				
				//OR
				else if(c == ';') {
					currentVariables.pop();
					currentVariables.push("");
					prologProgram.append("\n    ;\n    1=1");
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
