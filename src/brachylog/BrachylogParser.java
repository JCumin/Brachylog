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
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Stack;

import org.jpl7.Query;
import org.jpl7.Term;


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
		
		//Add an extra meaningless space tomake sure the last predicate is properly called (greater/less for example)
		program = program + " ";
		
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
		String negateNextPredicate = "";
		char previousChar = ' ';
		
		for(int charIndex = 0 ; charIndex < program.toCharArray().length ; charIndex++) {

			char c = program.toCharArray()[charIndex];

			StringBuilder currentRule = predicatesRules.get(currentPredicateIndex).get(currentRuleIndex);
			Stack<String> currentVariables = predicatesVariables.get(currentPredicateIndex).get(currentRuleIndex);
			int variableCounter = variableCounters.get(currentPredicateIndex).get(currentRuleIndex);
			
			//READING STRING
			if(currentString.length() > 0) {
				if(c == '\\') {
					if(escapeNextCharacter) {
						currentString.append("\\");
						escapeNextCharacter = false;
					} else {
						escapeNextCharacter = true;
						currentString.append("\\");
					}
				} else if(c == '"') {
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
							currentRule.append(",\n    " + negateNextPredicate + currentString.toString() + " = " + currentVariables.lastElement());
							negateNextPredicate = "";
						}
						currentString.setLength(0);
					} else {
						currentString.append(c);
						escapeNextCharacter = false;
					}
				} else {
					if(c == '\n') {
						currentString.append("\\").append("n");
					} else {
						currentString.append(c);	
					}
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

				if(c != '=' && (previousChar == '>' || previousChar == '<')) {
					if(previousChar == '<') {
						predicatesUsed.put("<", BrachylogPredicates.pLess());
						currentRule.append(",\n    " + negateNextPredicate + Constants.P_LESS + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					} else if(previousChar == '>') {
						predicatesUsed.put(">", BrachylogPredicates.pGreater());
						currentRule.append(",\n    " + negateNextPredicate + Constants.P_GREATER + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					previousChar = ' ';
				}
				
				//VARIABLE NAME
				if(Character.isUpperCase(c)) {
					String variableName = String.valueOf(c);
					if(previousChar == '@') {
						variableName = BrachylogAlphabetVariables.getValueForName(c);
						previousChar = ' ';
					} else if(previousChar == '$') {
						variableName = BrachylogMathVariables.getValueForName(c);
						previousChar = ' ';
					}
					if(currentVariables.lastElement().isEmpty()) {
						currentVariables.pop();
						currentVariables.push(variableName);
					} else if(lastCharIsColon || lastCharArithmetic || lastCharArithmeticParenthesis) {
						String s = currentVariables.pop();
						currentVariables.push(s + variableName);
						lastCharIsColon = false;
					} else {
						currentRule.append(",\n    " + negateNextPredicate + variableName + " = " + currentVariables.lastElement());
						negateNextPredicate = "";
					}
					continue;
				}
				
				//NUMBER
				if(Character.isDigit(c) && previousChar != '$' && previousChar != '@') {
					if(!readingNumber) {
						if(currentVariables.lastElement().isEmpty() || lastCharIsColon || lastCharArithmetic || lastCharArithmeticParenthesis) {
							fillNumberInCurrentVariable = true;
							lastCharIsColon = false;
							lastCharArithmetic = false;
							lastCharArithmeticParenthesis = false;
							String s = currentVariables.pop();
							currentVariables.push(s + c);
						} else {
							currentRule.append(",\n    " + negateNextPredicate + currentVariables.lastElement() + " = " + c);
							negateNextPredicate = "";
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
				if(c == '"' && previousChar != '$' && previousChar != '@') {
					currentString.append(c);
				}
				
				//START ARGS
				else if(c == ':' && previousChar != '$' && previousChar != '@') {
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
				else if(c == '[' && previousChar != '$' && previousChar != '@') {
					String s = currentVariables.pop();
					if(s.isEmpty()) {
						currentVariables.push("[");
						arrayOpened = true;
					} else if(arrayOpened || lastCharIsColon) {
						currentVariables.push(s + "[");
					} else {
						currentVariables.push(s);
						currentVariables.push("[");
						arrayOpened = true;
					}
					lastCharIsColon = true;
				}
				
				//END ARRAY
				else if(c == ']' && previousChar != '$' && previousChar != '@') {
					String s = currentVariables.pop();
					if(s.replace("]", "").length() - s.replace("[", "").length() == 1) {
						arrayOpened = false;
						if(currentVariables.size() > 0) {
							String previous = currentVariables.pop();
							currentRule.append(",\n    " + negateNextPredicate + s + "] = " + previous);
						}
					} else {
						arrayOpened = true;
					}
					currentVariables.push(s + "]");
				}
				
				//INPUT VARIABLE
				else if(c == '?' && previousChar != '$' && previousChar != '@') {
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
						currentRule.append(",\n    " + negateNextPredicate + Constants.V_INPUT + " = " + currentVariables.lastElement());
						negateNextPredicate = "";
					}
				}
				
				//OUTPUT VARIABLE
				else if(c == '.' && previousChar != '$' && previousChar != '@') {
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
							currentRule.append(",\n    " + negateNextPredicate + Constants.V_OUTPUT + " = " + currentVariables.lastElement());		
							negateNextPredicate = "";
						}
					}
				}
				
				//EMPTY LIST
				else if(c == 'q' && previousChar != '$' && previousChar != '@') {
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
							currentRule.append(",\n    " + negateNextPredicate + "[]" + " = " + currentVariables.lastElement());	
							negateNextPredicate = "";
						}
					}
				}
				
				//AND
				else if(c == ',' && previousChar != '$' && previousChar != '@') {
					currentVariables.pop();
					currentVariables.push("");
				}
				
				//OR
				else if(c == ';' && previousChar != '$' && previousChar != '@') {
					currentVariables.pop();
					currentVariables.push("");
					currentRule.append("\n    ;\n    1=1");
				}
				
				//CUT
				else if(c == '!' && previousChar != '$' && previousChar != '@') {
					currentRule.append(",\n    !");
				}
				
				//BACKTRACK
				else if (c == '\\' && previousChar != '$' && previousChar != '@') {
					currentRule.append(",\n    0 = 1");
				}
				
				//START INLINE PROLOG
				else if(c == '`' && previousChar != '$' && previousChar != '@') {
					currentVariables.pop();
					currentVariables.push("");
					currentRule.append(",\n    ");
					readingInlineProlog = true;
				}
				
				else if(c == '\'' && previousChar != '$' && previousChar != '@') {
					negateNextPredicate = " \\+ ";
					continue;
				}
				
				//ARITHMETIC
				else if((c == '+' || c == '-' || c == '*' || c == '/' || c == '%' || c == '^')
						 && previousChar != '$' && previousChar != '@') {
					String s = currentVariables.pop();
					if(c == '%') {
						currentVariables.push(s + " mod ");	
					} else {
						currentVariables.push(s + " " + c + " ");
					}
					lastCharArithmetic = true;
				}
				
				//OPEN PARENTHESIS
				else if(c == '(' && previousChar != '$' && previousChar != '@') {
					if(lastCharArithmetic || lastCharIsColon) {
						currentVariables.push("(");
						lastCharArithmeticParenthesis = true;
					} else {
						if(currentRule.toString().endsWith("(")) {
							currentRule.append(negateNextPredicate + "(");
							negateNextPredicate = "";
						}
						currentRule.append(",\n    " + negateNextPredicate + "( 1=1");
						negateNextPredicate = "";
					}
				}
				
				//CLOSE PARENTHESIS
				else if(c == ')' && previousChar != '$' && previousChar != '@') {
					if(currentVariables.size() > 1) {
						String s = currentVariables.pop();
						String s2 = currentVariables.pop();
						currentVariables.push(s2 + s + c);	
					} else {
						currentRule.append("\n    )");
					}
				}
				
				//START PREDICATE
				else if(c == '{' && previousChar != '$' && previousChar != '@') {
					if(currentVariables.size() <= 1) {
						lastCharIsColon = false;
						if(arrayOpened) {
							String s = currentVariables.pop();
							currentVariables.push(s + "]");
							arrayOpened = false;
						}	
					}
					
					String predicateName = Constants.P_SUBPREDICATE + predicatesRules.size();
					if(!currentVariables.lastElement().equals("")) {
						currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + currentVariables.lastElement() + ",V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);	
						negateNextPredicate = "";
					}
					
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
				else if(c =='}' && previousChar != '$' && previousChar != '@') {
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
				else if(c == '|' && previousChar != '$' && previousChar != '@') {
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
					if(c != '$') {
						lastCharArithmetic = false;
						lastCharArithmeticParenthesis = false;
					}
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
						currentRule.append(",\n    " + negateNextPredicate + Constants.P_BEHEAD + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//CONCATENATE
					else if(c == 'c') {
						if(previousChar == '$') {
							predicatesUsed.put("$c", BrachylogMathPredicates.pmCos());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_COS + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						} else {
							predicatesUsed.put("c", BrachylogPredicates.pConcatenate());
							currentRule.append(",\n    " + negateNextPredicate + Constants.P_CONCATENATE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//DUPLICATES
					else if(c == 'd') {
						predicatesUsed.put("d", BrachylogPredicates.pDuplicates());
						currentRule.append(",\n    " + negateNextPredicate + Constants.P_DUPLICATES + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//ENUMERATE
					else if(c == 'e') {
						if(previousChar == '$') {
							predicatesUsed.put("$e", BrachylogMathPredicates.pmExp());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_EXP + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						} else {
							predicatesUsed.put("e", BrachylogPredicates.pEnumerate());
							currentRule.append(",\n    " + negateNextPredicate + Constants.P_ENUMERATE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//FINDALL
					else if(c == 'f') {
						if(previousChar == '$') {

						} else {
							predicatesUsed.put("f", BrachylogPredicates.pFindAll());
							predicatesUsed.put("&", BrachylogPredicates.pCallPredicate());
							currentRule.append(",\n    " + negateNextPredicate + Constants.P_FINDALL + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//HEAD
					else if(c == 'h') {
						predicatesUsed.put("h", BrachylogPredicates.pHead());
						currentRule.append(",\n    " + negateNextPredicate + Constants.P_HEAD + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//LENGTH
					else if(c == 'l') {
						if(previousChar == '$') {
							predicatesUsed.put("$l", BrachylogMathPredicates.pmLog());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_LOG + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						} else if(previousChar == '@') {
							predicatesUsed.put("@l", BrachylogAlphabetPredicates.paLowercase());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PA_LOWERCASE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						} else {
							predicatesUsed.put("l", BrachylogPredicates.pLength());
							currentRule.append(",\n    " + negateNextPredicate + Constants.P_LENGTH + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//MEMBER
					else if(c == 'm') {
						predicatesUsed.put("m", BrachylogPredicates.pMember());
						currentRule.append(",\n    " + negateNextPredicate + Constants.P_MEMBER + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//SORT
					else if(c == 'o') {
						predicatesUsed.put("o", BrachylogPredicates.pOrder());
						currentRule.append(",\n    " + negateNextPredicate + Constants.P_ORDER + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//PERMUTE
					else if(c == 'p') {
						if(previousChar == '$') {
							predicatesUsed.put("$p", BrachylogMathPredicates.pmPrimeDecomposition());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_PRIMEDECOMPOSITION + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						} else {
							predicatesUsed.put("p", BrachylogPredicates.pPermute());
							currentRule.append(",\n    " + negateNextPredicate + Constants.P_PERMUTE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);	
						}
					}
					
					//REVERSE
					else if(c == 'r') {
						if(previousChar == '$') {
							predicatesUsed.put("$r", BrachylogMathPredicates.pmRoot());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_ROOT + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						} else {
							predicatesUsed.put("r", BrachylogPredicates.pReverse());
							currentRule.append(",\n    " + negateNextPredicate + Constants.P_REVERSE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//SUBSET
					else if(c == 's') {
						if(previousChar == '$') {
							predicatesUsed.put("$s", BrachylogMathPredicates.pmSin());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_SIN + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						} else {
							predicatesUsed.put("s", BrachylogPredicates.pSubset());
							currentRule.append(",\n    " + negateNextPredicate + Constants.P_SUBSET + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}

					//T
					else if(c == 't') {
						if(previousChar == '$') {
							predicatesUsed.put("$t", BrachylogMathPredicates.pmTan());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_TAN + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						} else {

						}
					}

					//U
					else if(c == 'u') {
						if(previousChar == '@') {
							predicatesUsed.put("@u", BrachylogAlphabetPredicates.paUppercase());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PA_UPPERCASE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						} else {

						}
					}
					
					//WRITE
					else if(c == 'w') {
						predicatesUsed.put("w", BrachylogPredicates.pWrite());
						currentRule.append(",\n    " + negateNextPredicate + Constants.P_WRITE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//XTERMINATE
					else if(c == 'x') {
						predicatesUsed.put("x", BrachylogPredicates.pXterminate());
						currentRule.append(",\n    " + negateNextPredicate + Constants.P_XTERMINATE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//CALL PREDICATE
					else if(c == '&') {
						predicatesUsed.put("&", BrachylogPredicates.pCallPredicate());
						currentRule.append(",\n    " + negateNextPredicate + Constants.P_CALLPREDICATE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					
					//ARITHMETIC EQUALITY OR COMPARISONS
					else if(c == '=') {
						if(previousChar == '<') {
							predicatesUsed.put("<=", BrachylogPredicates.pLessEqual());
							currentRule.append(",\n    " + negateNextPredicate + Constants.P_LESSEQUAL + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						} else if(previousChar == '>') {
							predicatesUsed.put(">=", BrachylogPredicates.pGreaterEqual());
							currentRule.append(",\n    " + negateNextPredicate + Constants.P_GREATEREQUAL + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						} else {
							currentRule.append(",\n    " + negateNextPredicate + "V" + variableCounter + " is " + currentVariables.lastElement());
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}

					// [
					else if(c == '[') {
						if(previousChar == '$') {
							predicatesUsed.put("$[", BrachylogMathPredicates.pmFloor());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_FLOOR + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						} else {

						}
					}

					// ]
					else if(c == ']') {
						if(previousChar == '$') {
							predicatesUsed.put("$]", BrachylogMathPredicates.pmCeil());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_CEIL + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//(
					else if(c == '(') {
						if(previousChar == '$') {
							predicatesUsed.put("$(", BrachylogMathPredicates.pmCircularPermuteLeft());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_CIRCULAR_PERM_LEFT + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//)
					else if(c == ')') {
						if(previousChar == '$') {
							predicatesUsed.put("$)", BrachylogMathPredicates.pmCircularPermuteRight());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_CIRCULAR_PERM_RIGHT + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//!
					else if(c == '!') {
						if(previousChar == '$') {
							predicatesUsed.put("$!", BrachylogMathPredicates.pmFactorial());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_FACTORIAL + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					///
					else if(c == '/') {
						if(previousChar == '$') {
							predicatesUsed.put("$/", BrachylogMathPredicates.pmAntiTranspose());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_ANTITRANSPOSE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//\
					else if(c == '\\') {
						if(previousChar == '$') {
							predicatesUsed.put("$\\", BrachylogMathPredicates.pmTranspose());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_TRANSPOSE + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//1
					else if(c == '1') {
						if(previousChar == '$') {
							predicatesUsed.put("$1", BrachylogMathPredicates.pmArcCos());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_ARCCOS + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//2
					else if(c == '2') {
						if(previousChar == '$') {
							predicatesUsed.put("$2", BrachylogMathPredicates.pmArcSin());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_ARCSIN + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//3
					else if(c == '3') {
						if(previousChar == '$') {
							predicatesUsed.put("$3", BrachylogMathPredicates.pmArcTan());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_ARCTAN + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//4
					else if(c == '4') {
						if(previousChar == '$') {
							predicatesUsed.put("$4", BrachylogMathPredicates.pmCosh());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_COSH + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//5
					else if(c == '5') {
						if(previousChar == '$') {
							predicatesUsed.put("$5", BrachylogMathPredicates.pmSinh());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_SINH + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//6
					else if(c == '6') {
						if(previousChar == '$') {
							predicatesUsed.put("$6", BrachylogMathPredicates.pmTanh());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_TANH + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//7
					else if(c == '7') {
						if(previousChar == '$') {
							predicatesUsed.put("$7", BrachylogMathPredicates.pmArcCosh());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_ARCCOSH + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//8
					else if(c == '8') {
						if(previousChar == '$') {
							predicatesUsed.put("$8", BrachylogMathPredicates.pmArcSinh());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_ARCSINH + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//9
					else if(c == '9') {
						if(previousChar == '$') {
							predicatesUsed.put("$9", BrachylogMathPredicates.pmArcTanh());
							currentRule.append(",\n    " + negateNextPredicate + Constants.PM_ARCTANH + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
							currentVariables.pop();
							currentVariables.push("V" + variableCounter++);
							variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
						}
					}
					
					//0
					else if(c == '0') {
						if(previousChar == '$') {
							
						}
					}

					if(c != '@' && c != '#' && c != '<' && c != '>' && c != '$') {
						negateNextPredicate = "";
					}
				}
			}
			previousChar = c;
		}
		predicatesRules.get(currentPredicateIndex).get(currentRuleIndex).append(".\n");
		
		StringBuilder prologProgram = new StringBuilder();
		
		//Deactivates singleton variables warnings (that we get on all the unused implicit variables)
		prologProgram.append(":- style_check(-singleton).\n");
		prologProgram.append("\n");
		
		//Add CLPFD library for certain built-ins
		prologProgram.append(":- use_module(library(clpfd)).\n");
		prologProgram.append("\n");
		
		//Concatenate all rules of all predicates one after the other
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
			
		    if((Query.allSolutions("consult('" + Constants.PROLOG_FILE + "')")).length > 0) {
		    	Term inputTerm = parseArg(input);
		    	Term outputTerm = parseArg(output);
		    	
		    	Query mainQuery = new Query(Constants.P_MAIN, new Term[] {inputTerm, outputTerm});
		    	boolean noNewSolution = true;
		    	while(mainQuery.hasMoreSolutions()) {
		    		Map<String, Term> bindings = mainQuery.nextSolution();
		    		if(bindings.entrySet().size() > 0){
			    		for(Map.Entry<String, Term> t : bindings.entrySet()) {
			    			try {
			    				String tToString = termListToString(t.getValue());
				    			System.out.println(t.getKey() + " = " + tToString);	
			    			} catch(Exception e) {
				    			System.out.println(t.getKey() + " = " + t.getValue());
			    			}
			    		}	
		    		} else {
		    			System.err.println("\nTrue.");
		    		}
		    		if(mainQuery.hasMoreSolutions()) {
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
		    		} else {
		    			noNewSolution = false;
		    			break;
		    		}
		    	}
		    	if(noNewSolution) {
		    		System.err.println("\nFalse.");
		    	}
		    	
		    } else {
		    	System.out.println("Could not open file " + Constants.PROLOG_FILE);
		    }
		}
	}
	
	
	private static Term parseArg(String arg) {
		Term term;
		String[] argSplit = arg.split(":");
		
		if(argSplit.length == 1) {
			term = org.jpl7.Util.textToTerm(arg);
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
			term = org.jpl7.Util.textToTerm(changedArg.toString());
		}
    	return term;
	}
	
	
	private static String termListToString(Term t) {
		StringBuilder s = new StringBuilder();
		Term[] args = t.toTermArray();
		for(Term arg : args) {
			String argString = "";
			try {
				argString += termListToString(arg);
				argString = "[" + argString + "]";
			} catch(Exception e) {
				argString += arg.toString();
			}
			s.append(argString);
			s.append(":");
		}
		s.deleteCharAt(s.length() - 1);
		
		return s.toString();
	}
	
	
	private static void savePrologToFile(String program) throws Exception {
		PrintWriter writer = new PrintWriter(Constants.PROLOG_FILE);
		writer.print(program);
		writer.close();
	}
	
}
