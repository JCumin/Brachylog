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
		boolean lastCharIsColon = false;
		boolean arrayOpened = false;
		boolean lastCharArithmetic = false;
		boolean lastCharArithmeticParenthesis = false;
		boolean readingInlineProlog = false;
		String negateNextPredicate = "";
		char previousChar = ' ';
		boolean reverseNextPredicate = false;
		String predicateName = "";
		String currentInputVariable = "";
		String currentNumber = "";
		
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
							if(!predicateName.isEmpty()) {
								if(reverseNextPredicate) {
									currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + currentString.toString() + ", " + currentInputVariable + ")");
									reverseNextPredicate = false;
								} else {
									currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + currentInputVariable + ", " + currentString.toString() + ")");
								}
								currentVariables.pop();
								currentVariables.push(currentString.toString());
								predicateName = "";
								currentInputVariable = "";
							} else {
								currentRule.append(",\n    " + negateNextPredicate + currentString.toString() + " = " + currentVariables.lastElement());
							}
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
						if(reverseNextPredicate) {
							currentRule.append(",\n    " + negateNextPredicate + Constants.P_LESS + "(" + "V" + variableCounter + ", " + currentVariables.lastElement() + ")");
							reverseNextPredicate = false;
						} else {
							currentRule.append(",\n    " + negateNextPredicate + Constants.P_LESS + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");							
						}
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					} else if(previousChar == '>') {
						predicatesUsed.put(">", BrachylogPredicates.pGreater());
						if(reverseNextPredicate) {
							currentRule.append(",\n    " + negateNextPredicate + Constants.P_GREATER + "(" + "V" + variableCounter + ", " + currentVariables.lastElement() + ")");
							reverseNextPredicate = false;
						} else {
							currentRule.append(",\n    " + negateNextPredicate + Constants.P_GREATER + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
						}
						currentVariables.pop();
						currentVariables.push("V" + variableCounter++);
						variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					}
					previousChar = ' ';
				}
				
				if(!predicateName.isEmpty() && c != '$' && c != '@' && !Character.isUpperCase(c) && (c != '"' || (previousChar == '$' || previousChar == '@')) &&
						(c != '[' || (previousChar == '$' || previousChar == '@')) && (!Character.isDigit(c) || (previousChar == '$' || previousChar == '@'))  &&
						(c != '?' || (previousChar == '$' || previousChar == '@')) && (c != '.' || (previousChar == '$' || previousChar == '@')) &&
						!arrayOpened && currentNumber.isEmpty()) {
					if(reverseNextPredicate) {
						currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + "V" + variableCounter + ", " + currentVariables.lastElement() + ")");
						reverseNextPredicate = false;
					} else {
						currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + currentVariables.lastElement() + ", V" + variableCounter + ")");
					}
					currentVariables.pop();
					currentVariables.push("V" + variableCounter++);
					variableCounters.get(currentPredicateIndex).set(currentRuleIndex, variableCounter);
					predicateName = "";
					currentInputVariable = "";
					negateNextPredicate = "";
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
						if(arrayOpened) {
							String s = currentVariables.pop();
							currentVariables.push(s + "]");
						}
						arrayOpened = false;
						lastCharIsColon = false;
						if(!predicateName.isEmpty()) {
							if(reverseNextPredicate) {
								currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + variableName + ", " + currentInputVariable + ")");
								reverseNextPredicate = false;
							} else {
								currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + currentInputVariable + ", " + variableName + ")");
							}
							currentVariables.pop();
							currentVariables.push(variableName);
							predicateName = "";
							currentInputVariable = "";
						} else {
							currentRule.append(",\n    " + negateNextPredicate + variableName + " = " + currentVariables.lastElement());	
						}
						negateNextPredicate = "";
					}
					continue;
				}
				
				//NUMBER
				if(Character.isDigit(c) && previousChar != '$' && previousChar != '@') {
					lastCharIsColon = false;
					currentNumber = currentNumber + c;
					continue;
				} else {
					if(!currentNumber.isEmpty() && c == '.') {
						currentNumber = currentNumber + c;
						continue;
					} else if (!currentNumber.isEmpty()){
						if(arrayOpened || lastCharArithmetic || lastCharArithmeticParenthesis || (predicateName.isEmpty() && currentVariables.lastElement().isEmpty())) {
							String s = currentVariables.pop();
							currentVariables.push(s + currentNumber);
							lastCharArithmetic = false;
							lastCharArithmeticParenthesis = false;
						} else {
							if(!predicateName.isEmpty()) {
								if(reverseNextPredicate) {
									currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + currentNumber + ", " + currentInputVariable + ")");
									reverseNextPredicate = false;
								} else {
									currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + currentInputVariable + ", " + currentNumber + ")");
								}
								currentVariables.pop();
								currentVariables.push(currentNumber);
								predicateName = "";
								currentInputVariable = "";
							} else {
								currentRule.append(",\n    " + negateNextPredicate + currentVariables.lastElement() + " = " + currentNumber);
							}
							negateNextPredicate = "";	
						}
						currentNumber = "";
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
							if(!predicateName.isEmpty()) {
								if(reverseNextPredicate) {
									currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + s + "]" + ", " + currentInputVariable + ")");
									reverseNextPredicate = false;
								} else {
									currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + currentInputVariable + ", " + s + "]" + ")");
								}
								predicateName = "";
								currentInputVariable = "";
							} else {
								currentRule.append(",\n    " + negateNextPredicate + s + "] = " + previous);
							}
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
						if(arrayOpened) {
							String s = currentVariables.pop();
							currentVariables.push(s + "]");
						}
						arrayOpened = false;
						lastCharIsColon = false;
						if(!predicateName.isEmpty()) {
							if(reverseNextPredicate) {
								currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + Constants.V_INPUT + ", " + currentInputVariable + ")");
								reverseNextPredicate = false;
							} else {
								currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + currentInputVariable + ", " + Constants.V_INPUT + ")");
							}
							currentVariables.pop();
							currentVariables.push(Constants.V_INPUT);
							predicateName = "";
							currentInputVariable = "";
						} else {
							currentRule.append(",\n    " + negateNextPredicate + Constants.V_INPUT + " = " + currentVariables.lastElement());
						}
						negateNextPredicate = "";
					}
				}
				
				//OUTPUT VARIABLE
				else if(c == '.' && previousChar != '$' && previousChar != '@') {
					if(currentNumber.isEmpty()) {
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
							if(arrayOpened) {
								String s = currentVariables.pop();
								currentVariables.push(s + "]");
							}
							arrayOpened = false;
							lastCharIsColon = false;
							if(!predicateName.isEmpty()) {
								if(reverseNextPredicate) {
									currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + Constants.V_OUTPUT + ", " + currentInputVariable + ")");
									reverseNextPredicate = false;
								} else {
									currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + currentInputVariable + ", " + Constants.V_OUTPUT + ")");
								}
								currentVariables.pop();
								currentVariables.push(Constants.V_OUTPUT);
								predicateName = "";
								currentInputVariable = "";
							} else {
								currentRule.append(",\n    " + negateNextPredicate + Constants.V_OUTPUT + " = " + currentVariables.lastElement());
							}
							negateNextPredicate = "";
						}
					}
				}
				
				//EMPTY LIST
				else if(c == '_' && previousChar != '$' && previousChar != '@') {
					if(currentNumber.isEmpty()) {
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
							if(!predicateName.isEmpty()) {
								if(reverseNextPredicate) {
									currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + "[]" + ", " + currentInputVariable + ")");
									reverseNextPredicate = false;
								} else {
									currentRule.append(",\n    " + negateNextPredicate + predicateName + "(" + currentInputVariable + ", " + "[]" + ")");
								}
								currentVariables.pop();
								currentVariables.push("[]");
								predicateName = "";
								currentInputVariable = "";
							} else {
								currentRule.append(",\n    " + negateNextPredicate + "[]" + " = " + currentVariables.lastElement());
							}
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
				
				//NEGATE PREDICATE
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
					
					String newPredicateName = Constants.P_SUBPREDICATE + predicatesRules.size();
					if(!currentVariables.lastElement().equals("")) {
						if(reverseNextPredicate) {
							currentRule.append(",\n    " + negateNextPredicate + newPredicateName + "(" + "V" + variableCounter + ", " + currentVariables.lastElement() + ")");
							reverseNextPredicate = false;
						} else {
							currentRule.append(",\n    " + negateNextPredicate + newPredicateName + "(" + currentVariables.lastElement() + ",V" + variableCounter + ")");
						}
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
					newRule.append(newPredicateName + "(" + Constants.V_INPUT + "," + Constants.V_OUTPUT + ") :-\n");
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
					String currentPredicateName;
					if(currentPredicateIndex == 0) {
						currentPredicateName = Constants.P_MAIN;
					} else {
						currentPredicateName = Constants.P_SUBPREDICATE + currentPredicateIndex;
					}
					newRule.append(currentPredicateName + "(" + Constants.V_INPUT + "," + Constants.V_OUTPUT + ") :-\n");
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
						if(c != '$' && c != '@') {
							lastCharIsColon = false;
							if(arrayOpened) {
								String s = currentVariables.pop();
								currentVariables.push(s + "]");
								arrayOpened = false;
							}
						}
					}
					
					//B
					if(c == 'b') {
						predicatesUsed.put("b", BrachylogPredicates.pBehead());
						predicateName = Constants.P_BEHEAD;
					}
					
					//C
					else if(c == 'c') {
						if(previousChar == '$') {
							predicatesUsed.put("$c", BrachylogMathPredicates.pmCos());
							predicateName = Constants.PM_COS ;
						} else {
							predicatesUsed.put("c", BrachylogPredicates.pConcatenate());
							predicateName = Constants.P_CONCATENATE;
						}
					}
					
					//D
					else if(c == 'd') {
						predicatesUsed.put("d", BrachylogPredicates.pDuplicates());
						predicateName = Constants.P_DUPLICATES;
					}
					
					//E
					else if(c == 'e') {
						if(previousChar == '$') {
							predicatesUsed.put("$e", BrachylogMathPredicates.pmExp());
							predicateName = Constants.PM_EXP;
						} else {
							predicatesUsed.put("e", BrachylogPredicates.pEnumerate());
							predicateName = Constants.P_ENUMERATE;
						}
					}
					
					//F
					else if(c == 'f') {
						if(previousChar == '$') {

						} else {
							predicatesUsed.put("f", BrachylogPredicates.pFindAll());
							predicatesUsed.put("&", BrachylogPredicates.pCallPredicate());
							predicateName = Constants.P_FINDALL;
						}
					}
					
					//H
					else if(c == 'h') {
						predicatesUsed.put("h", BrachylogPredicates.pHead());
						predicateName = Constants.P_HEAD;
					}
					
					//L
					else if(c == 'l') {
						if(previousChar == '$') {
							predicatesUsed.put("$l", BrachylogMathPredicates.pmLog());
							predicateName = Constants.PM_LOG;
						} else if(previousChar == '@') {
							predicatesUsed.put("@l", BrachylogAlphabetPredicates.paLowercase());
							predicateName = negateNextPredicate + Constants.PA_LOWERCASE;
						} else {
							predicatesUsed.put("l", BrachylogPredicates.pLength());
							predicateName = Constants.P_LENGTH;
						}
					}
					
					//M
					else if(c == 'm') {
						predicatesUsed.put("m", BrachylogPredicates.pMember());
						predicateName = Constants.P_MEMBER;
					}
					
					//S
					else if(c == 'o') {
						predicatesUsed.put("o", BrachylogPredicates.pOrder());
						predicateName = Constants.P_ORDER;
					}
					
					//P
					else if(c == 'p') {
						if(previousChar == '$') {
							predicatesUsed.put("$p", BrachylogMathPredicates.pmPrimeDecomposition());
							predicateName = Constants.PM_PRIMEDECOMPOSITION;
						} else {
							predicatesUsed.put("p", BrachylogPredicates.pPermute());
							predicateName = Constants.P_PERMUTE;
						}
					}
					
					//R
					else if(c == 'r') {
						if(previousChar == '$') {
							predicatesUsed.put("$r", BrachylogMathPredicates.pmRoot());
							predicateName = Constants.PM_ROOT;
						} else {
							predicatesUsed.put("r", BrachylogPredicates.pReverse());
							predicateName = Constants.P_REVERSE;
						}
					}
					
					//S
					else if(c == 's') {
						if(previousChar == '$') {
							predicatesUsed.put("$s", BrachylogMathPredicates.pmSin());
							predicateName = Constants.PM_SIN;
						} else {
							predicatesUsed.put("s", BrachylogPredicates.pSubset());
							predicateName = Constants.P_SUBSET;
						}
					}

					//T
					else if(c == 't') {
						if(previousChar == '$') {
							predicatesUsed.put("$t", BrachylogMathPredicates.pmTan());
							predicateName = Constants.PM_TAN;
						} else {

						}
					}

					//U
					else if(c == 'u') {
						if(previousChar == '@') {
							predicatesUsed.put("@u", BrachylogAlphabetPredicates.paUppercase());
							predicateName = Constants.PA_UPPERCASE;
						} else {

						}
					}
					
					//W
					else if(c == 'w') {
						predicatesUsed.put("w", BrachylogPredicates.pWrite());
						predicateName = Constants.P_WRITE;
					}
					
					//X
					else if(c == 'x') {
						predicatesUsed.put("x", BrachylogPredicates.pXterminate());
						predicateName = Constants.P_XTERMINATE;
					}
					
					//&
					else if(c == '&') {
						predicatesUsed.put("&", BrachylogPredicates.pCallPredicate());
						predicateName = Constants.P_CALLPREDICATE;
					}
					
					//= and < >
					else if(c == '=') {
						if(previousChar == '<') {
							predicatesUsed.put("<=", BrachylogPredicates.pLessEqual());
							predicateName = Constants.P_LESSEQUAL;
						} else if(previousChar == '>') {
							predicatesUsed.put(">=", BrachylogPredicates.pGreaterEqual());
							predicateName = Constants.P_GREATEREQUAL;
						} else {
							predicatesUsed.put("=", BrachylogPredicates.pEqual());
							predicateName = Constants.P_EQUAL;
						}
					}

					// [
					else if(c == '[') {
						if(previousChar == '$') {
							predicatesUsed.put("$[", BrachylogMathPredicates.pmFloor());
							predicateName = Constants.PM_FLOOR;
						} else {

						}
					}

					// ]
					else if(c == ']') {
						if(previousChar == '$') {
							predicatesUsed.put("$]", BrachylogMathPredicates.pmCeil());
							predicateName = Constants.PM_CEIL;
						}
					}
					
					//(
					else if(c == '(') {
						if(previousChar == '$') {
							predicatesUsed.put("$(", BrachylogMathPredicates.pmCircularPermuteLeft());
							predicateName = Constants.PM_CIRCULAR_PERM_LEFT;
						}
					}
					
					//)
					else if(c == ')') {
						if(previousChar == '$') {
							predicatesUsed.put("$)", BrachylogMathPredicates.pmCircularPermuteRight());
							predicateName = Constants.PM_CIRCULAR_PERM_RIGHT;
						}
					}
					
					//!
					else if(c == '!') {
						if(previousChar == '$') {
							predicatesUsed.put("$!", BrachylogMathPredicates.pmFactorial());
							predicateName = Constants.PM_FACTORIAL;
						}
					}
					
					///
					else if(c == '/') {
						if(previousChar == '$') {
							predicatesUsed.put("$/", BrachylogMathPredicates.pmAntiTranspose());
							predicateName = Constants.PM_ANTITRANSPOSE;
						}
					}
					
					//\
					else if(c == '\\') {
						if(previousChar == '$') {
							predicatesUsed.put("$\\", BrachylogMathPredicates.pmTranspose());
							predicateName = Constants.PM_TRANSPOSE;
						}
					}
					
					//1
					else if(c == '1') {
						if(previousChar == '$') {
							predicatesUsed.put("$1", BrachylogMathPredicates.pmArcCos());
							predicateName = Constants.PM_ARCCOS;
						}
					}
					
					//2
					else if(c == '2') {
						if(previousChar == '$') {
							predicatesUsed.put("$2", BrachylogMathPredicates.pmArcSin());
							predicateName = Constants.PM_ARCSIN;
						}
					}
					
					//3
					else if(c == '3') {
						if(previousChar == '$') {
							predicatesUsed.put("$3", BrachylogMathPredicates.pmArcTan());
							predicateName = Constants.PM_ARCTAN;
						}
					}
					
					//4
					else if(c == '4') {
						if(previousChar == '$') {
							predicatesUsed.put("$4", BrachylogMathPredicates.pmCosh());
							predicateName = Constants.PM_COSH;
						}
					}
					
					//5
					else if(c == '5') {
						if(previousChar == '$') {
							predicatesUsed.put("$5", BrachylogMathPredicates.pmSinh());
							predicateName = Constants.PM_SINH;
						}
					}
					
					//6
					else if(c == '6') {
						if(previousChar == '$') {
							predicatesUsed.put("$6", BrachylogMathPredicates.pmTanh());
							predicateName = Constants.PM_TANH;
						}
					}
					
					//7
					else if(c == '7') {
						if(previousChar == '$') {
							predicatesUsed.put("$7", BrachylogMathPredicates.pmArcCosh());
							predicateName = Constants.PM_ARGCOSH;
						}
					}
					
					//8
					else if(c == '8') {
						if(previousChar == '$') {
							predicatesUsed.put("$8", BrachylogMathPredicates.pmArcSinh());
							predicateName = Constants.PM_ARGSINH;
						}
					}
					
					//9
					else if(c == '9') {
						if(previousChar == '$') {
							predicatesUsed.put("$9", BrachylogMathPredicates.pmArcTanh());
							predicateName = Constants.PM_ARGTANH;
						}
					}
					
					//0
					else if(c == '0') {
						if(previousChar == '$') {
							
						}
					}
					
					else if(c == '~') {
						if(previousChar == '$') {
							
						} else if(previousChar == '@') {
							
						} else {
							reverseNextPredicate = true;
						}
					}
					
					if(!predicateName.isEmpty()) {
						currentInputVariable = currentVariables.lastElement();
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
