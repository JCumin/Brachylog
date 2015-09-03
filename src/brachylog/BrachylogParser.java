package brachylog;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Stack;

import jpl.Atom;
import jpl.Compound;
import jpl.Query;
import jpl.Term;

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
		
		
		List<List<StringBuilder>> predicatesRules = new ArrayList<List<StringBuilder>>();
		
		predicatesRules.add(new ArrayList<StringBuilder>());
		int currentPredicateIndex = 0;
		predicatesRules.get(currentPredicateIndex).add(new StringBuilder());
		int currentRuleIndex = 0;
		
		predicatesRules.get(currentPredicateIndex).get(currentRuleIndex).append(Constants.P_MAIN + "(" + Constants.V_INPUT + "," + Constants.V_OUTPUT + ") :-\n");
		predicatesRules.get(currentPredicateIndex).get(currentRuleIndex).append("    1=1");
		
		List<List<Stack<String>>> predicatesVariables = new ArrayList<List<Stack<String>>>();
		predicatesVariables.add(new ArrayList<Stack<String>>());
		predicatesVariables.get(currentPredicateIndex).add(new Stack<String>());
		predicatesVariables.get(currentPredicateIndex).get(currentRuleIndex).push(Constants.V_INPUT);
		
		List<List<Integer>> variableCounters = new ArrayList<List<Integer>>();
		variableCounters.add(new ArrayList<Integer>());
		variableCounters.get(currentPredicateIndex).add(0);
		
		Stack<int[]> callingRule = new Stack<int[]>();

		Map<String, String> predicatesUsed = new HashMap<String, String>();
		boolean escapeNextCharacter = false;
		StringBuilder currentString = new StringBuilder();
		boolean readingNumber = false;
		boolean fillNumberInCurrentVariable = false;
		boolean lastCharIsColon = false;
		boolean arrayOpened = false;
		boolean lastCharArithmetic = false;
		boolean lastCharArithmeticParenthesis = false;
		boolean readingInlineProlog = false;
		
		for(char c : program.toCharArray()) {
			
			StringBuilder currentRule = predicatesRules.get(currentPredicateIndex).get(currentRuleIndex);
			Stack<String> currentVariables = predicatesVariables.get(currentPredicateIndex).get(currentRuleIndex);
			int variableCounter = variableCounters.get(currentPredicateIndex).get(currentRuleIndex);
			
			//READING STRING
			if(currentString.length() > 0) {
				if(c == '\\') {
					escapeNextCharacter = true;
					currentString.append("\\");
				} else if(c == '\'') {
					if(!escapeNextCharacter) {
						currentString.append(c);
						if(currentVariables.lastElement().isEmpty()) {
							currentVariables.pop();
							currentVariables.push(currentString.toString());
						} else if(lastCharIsColon) {
							String s = currentVariables.pop();
							currentVariables.push(s + currentString.toString());
							lastCharIsColon = false;
						} else {
							currentRule.append(",\n    " + currentString.toString() + " = " + currentVariables.lastElement());
						}
						currentString.setLength(0);
					} else {
						currentString.append(c);
						escapeNextCharacter = false;
					}
				} else {
					currentString.append(c);
				}
			} 
			
			//INLINE PROLOG
			else if(readingInlineProlog) {
				if(c == '`') {
					readingInlineProlog = false;
				} else {
					currentRule.append(c);	
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
						currentRule.append(",\n    " + c + " = " + currentVariables.lastElement());
					}
					continue;
				}
				
				//NUMBER
				if(Character.isDigit(c)) {
					if(!readingNumber) {
						if(currentVariables.lastElement().isEmpty() || lastCharIsColon || lastCharArithmetic || lastCharArithmeticParenthesis) {
							fillNumberInCurrentVariable = true;
							lastCharIsColon = false;
							lastCharArithmetic = false;
							lastCharArithmeticParenthesis = false;
							String s = currentVariables.pop();
							currentVariables.push(s + c);
						} else {
							currentRule.append(",\n    " + currentVariables.lastElement() + " = " + c);
						}
						readingNumber = true;
					} else {
						if(fillNumberInCurrentVariable) {
							String s = currentVariables.pop();
							currentVariables.push(s + c);
						} else {
							currentRule.append(c);
						}
					}
					continue;
				} else {
					if(readingNumber && c == '.') {
						if(fillNumberInCurrentVariable) {
							String s = currentVariables.pop();
							currentVariables.push(s + c);
						} else {
							currentRule.append(c);
						}
						continue;
					} else {
						readingNumber = false;
						fillNumberInCurrentVariable = false;
					}
				}
				
				//START STRING
				if(c == '\'') {
					currentString.append(c);
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
					} else {
						arrayOpened = true;
					}
					currentVariables.push(s + "]");
				}
				
				//INPUT VARIABLE
				else if(c == '?') {
					if(currentVariables.lastElement().isEmpty()) {
						currentVariables.pop();
						currentVariables.push(Constants.V_INPUT);
					} else if(lastCharIsColon || lastCharArithmetic || lastCharArithmeticParenthesis) {
						String s = currentVariables.pop();
						currentVariables.push(s + Constants.V_INPUT);
						lastCharIsColon = false;
						lastCharArithmetic = false;
						lastCharArithmeticParenthesis = false;
					} else {
						arrayOpened = false;
						lastCharIsColon = false;
						if(arrayOpened) {
							String s = currentVariables.pop();
							currentVariables.push(s + "]");
						}
						currentRule.append(",\n    " + Constants.V_INPUT + " = " + currentVariables.lastElement());
					}
				}
				
				//OUTPUT VARIABLE
				else if(c == '.') {
					if(!readingNumber) {
						if(currentVariables.lastElement().isEmpty()) {
							currentVariables.pop();
							currentVariables.push(Constants.V_OUTPUT);
						} else if(lastCharIsColon || lastCharArithmetic || lastCharArithmeticParenthesis) {
							String s = currentVariables.pop();
							currentVariables.push(s + Constants.V_OUTPUT);
							lastCharIsColon = false;
							lastCharArithmetic = false;
							lastCharArithmeticParenthesis = false;
						} else {
							arrayOpened = false;
							lastCharIsColon = false;
							if(arrayOpened) {
								String s = currentVariables.pop();
								currentVariables.push(s + "]");
							}
							currentRule.append(",\n    " + Constants.V_OUTPUT + " = " + currentVariables.lastElement());		
						}
					}
				}
				
				//OUTPUT VARIABLE
				else if(c == 'q') {
					if(!readingNumber) {
						if(currentVariables.lastElement().isEmpty()) {
							currentVariables.pop();
							currentVariables.push("[]");
						} else if(lastCharIsColon || lastCharArithmetic || lastCharArithmeticParenthesis) {
							String s = currentVariables.pop();
							currentVariables.push(s + "[]");
							lastCharIsColon = false;
							lastCharArithmetic = false;
							lastCharArithmeticParenthesis = false;
						} else {
							arrayOpened = false;
							lastCharIsColon = false;
							if(arrayOpened) {
								String s = currentVariables.pop();
								currentVariables.push(s + "]");
							}
							currentRule.append(",\n    " + "[]" + " = " + currentVariables.lastElement());		
						}
					}
				}
				
				//AND
				else if(c == ',') {
					currentVariables.pop();
					currentVariables.push("");
				}
				
				//OR
				else if(c == ';') {
					currentVariables.pop();
					currentVariables.push("");
					currentRule.append("\n    ;\n    1=1");
				}
				
				//CUT
				else if(c == '!') {
					currentRule.append(",\n    !");
				}
				
				//BACKTRACK
				else if (c == '\\') {
					currentRule.append(",\n    0 = 1");
				}
				
				//START INLINE PROLOG
				else if(c == '`') {
					currentVariables.pop();
					currentVariables.push("");
					currentRule.append(",\n    ");
					readingInlineProlog = true;
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
					currentRule.append(",\n    V" + variableCounter + " is " + currentVariables.lastElement());
					currentVariables.pop();
					currentVariables.push("V" + variableCounter++);
					variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
				}
				
				//OPEN PARENTHESIS
				else if(c == '(') {
					if(lastCharArithmetic || lastCharIsColon) {
						currentVariables.push("(");
						lastCharArithmeticParenthesis = true;
					} else {
						if(currentRule.toString().endsWith("(")) {
							currentRule.append("(");
						}
						currentRule.append(",\n    ( 1=1");
					}
				}
				
				//CLOSE PARENTHESIS
				else if(c == ')') {
					if(currentVariables.size() > 1) {
						String s = currentVariables.pop();
						String s2 = currentVariables.pop();
						currentVariables.push(s2 + s + c);	
					} else {
						currentRule.append("\n    )");
					}
				}
				
				//START PREDICATE
				else if(c == '{') {
					if(currentVariables.size() <= 1) {
						lastCharIsColon = false;
						if(arrayOpened) {
							String s = currentVariables.pop();
							currentVariables.push(s + "]");
							arrayOpened = false;
						}	
					}
					
					String predicateName = Constants.P_SUBPREDICATE + predicatesRules.size();
					currentRule.append(",\n    " + predicateName + "(" + currentVariables.lastElement() + ",V" + variableCounter + ")");
					currentVariables.pop();
					currentVariables.push("V" + variableCounter++);
					variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					
					callingRule.push(new int[] {currentPredicateIndex, currentRuleIndex});
					StringBuilder newRule = new StringBuilder();
					Stack<String> newVariables = new Stack<String>();
					List<StringBuilder> newPredicate = new ArrayList<StringBuilder>();
					List<Stack<String>> newPredVars = new ArrayList<Stack<String>>();
					List<Integer> newVarCounter = new ArrayList<Integer>();
					newPredicate.add(newRule);
					newPredVars.add(newVariables);
					variableCounters.add(newVarCounter);
					predicatesRules.add(newPredicate);
					predicatesVariables.add(newPredVars);
					
					currentPredicateIndex = predicatesRules.size() - 1;
					newVarCounter.add(0);
					currentRuleIndex = 0;
					newRule.append(predicateName + "(" + Constants.V_INPUT + "," + Constants.V_OUTPUT + ") :-\n");
					newRule.append("    1=1");
					
					newVariables.push(Constants.V_INPUT);
				}
				
				//END PREDICATE
				else if(c =='}') {
					if(currentVariables.size() <= 1) {
						lastCharIsColon = false;
						if(arrayOpened) {
							String s = currentVariables.pop();
							currentVariables.push(s + "]");
							arrayOpened = false;
						}	
					}
					
					currentRule.append(".\n");
					int[] prevRule = callingRule.pop();
					currentPredicateIndex = prevRule[0];
					currentRuleIndex = prevRule[1];
				}
				
				//START NEW RULE
				else if(c == '|') {
					if(currentVariables.size() <= 1) {
						lastCharIsColon = false;
						if(arrayOpened) {
							String s = currentVariables.pop();
							currentVariables.push(s + "]");
							arrayOpened = false;
						}	
					}
					
					currentRule.append(".\n");
					
					StringBuilder newRule = new StringBuilder();
					Stack<String> newVariables = new Stack<String>();
					predicatesRules.get(currentPredicateIndex).add(newRule);
					predicatesVariables.get(currentPredicateIndex).add(newVariables);
					variableCounters.get(currentPredicateIndex).add(0);
					
					currentRuleIndex = predicatesRules.get(currentPredicateIndex).size() - 1;
					String predicateName;
					if(currentPredicateIndex == 0) {
						predicateName = Constants.P_MAIN;
					} else {
						predicateName = Constants.P_SUBPREDICATE + currentPredicateIndex;
					}
					newRule.append(predicateName + "(" + Constants.V_INPUT + "," + Constants.V_OUTPUT + ") :-\n");
					newRule.append("    1=1");
					
					newVariables.push(Constants.V_INPUT);
				}
				
				
				//##########
				//PREDICATES
				//##########
				else {
					
					if(currentVariables.size() <= 1) {
						lastCharIsColon = false;
						if(arrayOpened) {
							String s = currentVariables.pop();
							currentVariables.push(s + "]");
							arrayOpened = false;
						}	
					}

					//BEHEAD
					if(c == 'b') {
						predicatesUsed.put("b", BrachylogPredicates.pBehead());
						currentRule.append(",\n    " + Constants.P_BEHEAD + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//CONCATENATE
					else if(c == 'c') {
						predicatesUsed.put("c", BrachylogPredicates.pConcatenate());
						currentRule.append(",\n    " + Constants.P_CONCATENATE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//ENUMERATE
					else if(c == 'e') {
						predicatesUsed.put("e", BrachylogPredicates.pEnumerate());
						currentRule.append(",\n    " + Constants.P_ENUMERATE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//HEAD
					else if(c == 'h') {
						predicatesUsed.put("h", BrachylogPredicates.pHead());
						currentRule.append(",\n    " + Constants.P_HEAD + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//LENGTH
					else if(c == 'l') {
						predicatesUsed.put("l", BrachylogPredicates.pLength());
						currentRule.append(",\n    " + Constants.P_LENGTH + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//PERMUTE
					else if(c == 'p') {
						predicatesUsed.put("p", BrachylogPredicates.pPermute());
						currentRule.append(",\n    " + Constants.P_PERMUTE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//REVERSE
					else if(c == 'r') {
						predicatesUsed.put("r", BrachylogPredicates.pReverse());
						currentRule.append(",\n    " + Constants.P_REVERSE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//SORT
					else if(c == 's') {
						predicatesUsed.put("p", BrachylogPredicates.pSort());
						currentRule.append(",\n    " + Constants.P_SORT + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//CALL PREDICATE
					else if(c == '&') {
						predicatesUsed.put("&", BrachylogPredicates.pCallPredicate());
						currentRule.append(",\n    " + Constants.P_CALLPREDICATE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
				}

			}
		}
		predicatesRules.get(currentPredicateIndex).get(currentRuleIndex).append(".\n");
		
		StringBuilder prologProgram = new StringBuilder();
		
		for(List<StringBuilder> l : predicatesRules) {
			for(StringBuilder s : l) {
				prologProgram.append(s.toString() + "\n");
			}
			prologProgram.append("\n");
		}
		for(Entry<String, String> e : predicatesUsed.entrySet()) {
			prologProgram.append(e.getValue());
		}
		
		return prologProgram.toString();
	}
	
	
	private static void run(String program, String input, String output) {
		if(program != null && !program.equals("")) {
			try {
				savePrologToFile(program);
			} catch(Exception e) {
				System.err.println("Failed to save temporary Prolog file.");
				return;
			}
			
		    Query consultQuery = new Query("consult", new Term[] {new Atom(Constants.PROLOG_FILE)});
		    if(consultQuery.hasSolution()) {
		    	Term inputTerm = parseArg(input);
		    	Term outputTerm = parseArg(output);

		    	Query mainQuery = new Query(new Compound(Constants.P_MAIN, new Term[] {inputTerm, outputTerm}));
		    	boolean noNewSolution = true;
		    	while(mainQuery.hasMoreSolutions()) {
		    		Hashtable<String, Term> bindings = mainQuery.nextSolution();
		    		if(bindings.entrySet().size() == 0) {
		    			System.out.println("True.");
		    		} else {
			    		for(Map.Entry<String, Term> t : bindings.entrySet()) {
			    			System.out.println(t.getKey() + " = " + t.getValue());
			    		}	
		    		}
		    		BufferedReader consoleIn = new BufferedReader(new InputStreamReader(System.in));
		    		String choice = "";
					try {
						choice = consoleIn.readLine();
					} catch (IOException e) {
						e.printStackTrace();
					}
		    		if(choice.contains(";")) {
		    			continue;
		    		} else {
		    			noNewSolution = false;
		    			break;
		    		}
		    	}
		    	if(noNewSolution) {
		    		System.out.println("False.");
		    	}
		    	
		    	System.out.println("\n--- END ---");
		    	
		    } else {
		    	System.out.println("Could not open file " + Constants.PROLOG_FILE);
		    }
		}
	}
	
	
	private static Term parseArg(String arg) {
		Term term;
		String[] argSplit = arg.split(":");
		
		if(argSplit.length == 1) {
			term = jpl.Util.textToTerm(arg);
		} else {
			StringBuilder changedArg = new StringBuilder();
			changedArg.append("[");
			for(String s : argSplit) {
				if(!s.equals("")) {
					changedArg.append(s);
					changedArg.append(",");
				}
			}
			changedArg.deleteCharAt(changedArg.length() - 1);
			changedArg.append("]");
			term = jpl.Util.textToTerm(changedArg.toString());
		}
    	return term;
	}
	
	
	private static void savePrologToFile(String program) throws Exception {
		PrintWriter writer = new PrintWriter(Constants.PROLOG_FILE);
		writer.print(program);
		writer.close();
	}
	
}
