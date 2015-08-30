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

public abstract class BrachylogParser {

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
		boolean fillNumberInCurrentVariable = false;
		boolean lastCharIsColon = false;
		boolean arrayOpened = false;
		boolean lastCharArithmetic = false;
		boolean lastCharParenthesis = false;
		
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
						} else if(lastCharIsColon) {
							String s = currentVariables.pop();
							currentVariables.push(s + currentString.toString());
							lastCharIsColon = false;
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
					} else if(lastCharIsColon) {
						String s = currentVariables.pop();
						currentVariables.push(s + c);
						lastCharIsColon = false;
					} else {
						prologProgram.append(",\n    " + c + " = " + currentVariables.lastElement());
					}
					continue;
				}
				
				//NUMBER
				if(Character.isDigit(c)) {
					if(!readingNumber) {
						if(currentVariables.lastElement().isEmpty() || lastCharIsColon || lastCharArithmetic || lastCharParenthesis) {
							fillNumberInCurrentVariable = true;
							lastCharIsColon = false;
							lastCharArithmetic = false;
							lastCharParenthesis = false;
							String s = currentVariables.pop();
							currentVariables.push(s + c);
						} else {
							prologProgram.append(",\n    " + currentVariables.lastElement() + " = " + c);
						}
						readingNumber = true;
					} else {
						if(fillNumberInCurrentVariable) {
							String s = currentVariables.pop();
							currentVariables.push(s + c);
						} else {
							prologProgram.append(c);
						}
					}
					continue;
				} else {
					if(readingNumber && c == '.') {
						if(fillNumberInCurrentVariable) {
							String s = currentVariables.pop();
							currentVariables.push(s + c);
						} else {
							prologProgram.append(c);
						}
						continue;
					} else {
						readingNumber = false;
						fillNumberInCurrentVariable = false;
					}
				}
				
				//START STRING
				if(c == '"') {
					currentString.append("\"");
				}
				
				//START ARGS
				else if(c == ':') {
					String s = currentVariables.pop();
					if(s.isEmpty()) {
						currentVariables.push("[");	
						arrayOpened = true;
					} else if(arrayOpened) {
						currentVariables.push(s + ",");
					} else {
						currentVariables.push("[" + s + ",");
						arrayOpened = true;
					}
					lastCharIsColon = true;
				}
				
				//START ARRAY
				else if(c == '[') {
					String s = currentVariables.pop();
					if(s.isEmpty()) {
						currentVariables.push("[");	
						arrayOpened = true;
					} else if(arrayOpened || lastCharIsColon) {
						currentVariables.push(s + "[");
					} else {
						currentVariables.push("[" + s + ",");
						arrayOpened = true;
					}
					lastCharIsColon = true;
				}
				
				//END ARRAY
				else if(c == ']') {
					String s = currentVariables.pop();
					if(s.replace("[", "").length() - s.replace("]", "").length() == 1) {
						arrayOpened = false;
					}
					currentVariables.push(s + "]");
				}
				
				//INPUT VARIABLE
				else if(c == '?') {
					if(currentVariables.lastElement().isEmpty()) {
						currentVariables.pop();
						currentVariables.push(Constants.V_INPUT);
					} else if(lastCharIsColon || lastCharArithmetic) {
						String s = currentVariables.pop();
						currentVariables.push(s + Constants.V_INPUT);
						lastCharIsColon = false;
						lastCharArithmetic = false;
					} else {
						arrayOpened = false;
						lastCharIsColon = false;
						if(currentVariables.lastElement().startsWith("[") && !currentVariables.lastElement().endsWith("]")) {
							String s = currentVariables.pop();
							currentVariables.push(s + "]");
						}
						prologProgram.append(",\n    " + Constants.V_INPUT + " = " + currentVariables.lastElement());
					}
				}
				
				//OUTPUT VARIABLE
				else if(c == '.') {
					if(!readingNumber) {
						if(currentVariables.lastElement().isEmpty()) {
							currentVariables.pop();
							currentVariables.push(Constants.V_OUTPUT);
						} else if(lastCharIsColon || lastCharArithmetic) {
							String s = currentVariables.pop();
							currentVariables.push(s + Constants.V_OUTPUT);
							lastCharIsColon = false;
							lastCharArithmetic = false;
						} else {
							arrayOpened = false;
							lastCharIsColon = false;
							if(currentVariables.lastElement().startsWith("[") && !currentVariables.lastElement().endsWith("]")) {
								String s = currentVariables.pop();
								currentVariables.push(s + "]");
							}
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
				
				//ARITHMETIC
				else if(c == '+' || c == '-' || c == '*' || c == '/' || c == '%' || c == '^') {
					String s = currentVariables.pop();
					if(c == '%') {
						currentVariables.push(s + " mod ");	
					} else {
						currentVariables.push(s + " " + c + " ");
					}
					lastCharArithmetic = true;
				}
				
				//ARITHMETIC EQUALITY
				else if(c == '=') {
					prologProgram.append(",\n    V" + variableCounter + " is " + currentVariables.lastElement());
					currentVariables.pop();
					currentVariables.push("V" + variableCounter++);
				}
				
				//PARENTHESIS
				else if(c == '(') {
					if(lastCharArithmetic || lastCharIsColon) {
						currentVariables.push("(");
					} else {
						String s = currentVariables.lastElement();
						currentVariables.pop();
						currentVariables.push("");
						currentVariables.push(c + s);
					}
					lastCharParenthesis = true;
				}
				
				else if(c == ')') {
					String s = currentVariables.pop();
					String s2 = currentVariables.pop();
					currentVariables.push(s2 + s + c);
				}
				
				//##########
				//PREDICATES
				//##########
				else {
					
					if(currentVariables.size() <= 1) {
						arrayOpened = false;
						lastCharIsColon = false;
						if(currentVariables.lastElement().startsWith("[") && !currentVariables.lastElement().endsWith("]")) {
							String s = currentVariables.pop();
							currentVariables.push(s + "]");
						}	
					}

					//BEHEAD
					if(c == 'b') {
						predicatesUsed.put("b", BrachylogPredicates.pBehead());
						prologProgram.append(",\n    " + Constants.P_BEHEAD + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
					}
					
					//CONCATENATE
					else if(c == 'c') {
						predicatesUsed.put("c", BrachylogPredicates.pConcatenate());
						prologProgram.append(",\n    " + Constants.P_CONCATENATE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
					}
					
					//HEAD
					else if(c == 'h') {
						predicatesUsed.put("h", BrachylogPredicates.pHead());
						prologProgram.append(",\n    " + Constants.P_HEAD + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
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
