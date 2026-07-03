package example;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

// minimal, zero-dependency calculator parser in Java 
// using a simple Recursive Descent Parser to handle operator precedence (like \(+\), \(-\)) and left-associativity

public class MinimalCalculator {

	enum Type {
		NUMBER, PLUS, MINUS, MUL, DIV, EOF
	}

	static class Token {
		Type type;
		String text;

		Token(Type type, String text) {
			this.type = type;
			this.text = text;
		}
	}

	// Tokenize the input string
	public static List<Token> tokenize(String input) {
		List<Token> tokens = new ArrayList<>();
		Pattern pattern = Pattern.compile("(\\s+)|([0-9]+)|(\\+)|(-)|(\\*)|(/)");
		Matcher matcher = pattern.matcher(input);

		while (matcher.find()) {
			if (matcher.group(1) != null)
				continue; // skip spaces
			if (matcher.group(2) != null)
				tokens.add(new Token(Type.NUMBER, matcher.group(2)));
			if (matcher.group(3) != null)
				tokens.add(new Token(Type.PLUS, matcher.group(3)));
			if (matcher.group(4) != null)
				tokens.add(new Token(Type.MINUS, matcher.group(4)));
			if (matcher.group(5) != null)
				tokens.add(new Token(Type.MUL, matcher.group(5)));

			if (matcher.group(6) != null)
				tokens.add(new Token(Type.DIV, matcher.group(6)));
		}
		tokens.add(new Token(Type.EOF, ""));
		return tokens;
	}

	static class Parser {
		List<Token> tokens;
		int pos = 0;

		Parser(List<Token> tokens) {
			this.tokens = tokens;
		}

		Token peek() {
			return tokens.get(pos);
		}

		Token consume() {
			return tokens.get(pos++);
		}

		// Primary -> NUMBER
		public int parsePrimary() {
			Token token = consume();
			return Integer.parseInt(token.text);
		}

		public int parseTerm() {
			int result = parsePrimary();

			while (peek().type == Type.MUL || peek().type == Type.DIV) {

				Token op = consume();
				int next = parsePrimary();

				if (op.type == Type.MUL) {
					result *= next;
				} else {
					result /= next;
				}
			}

			return result;
		}

		// Expression -> Primary ( (PLUS|MINUS) Primary )*
		public int parseExpression() {
			int result = parseTerm();

			while (peek().type == Type.PLUS || peek().type == Type.MINUS) {

				Token op = consume();
				int next = parseTerm();

				if (op.type == Type.PLUS) {
					result += next;
				} else {
					result -= next;
				}
			}

			return result;
		}

	}

	public static void main(String[] args) {
		Map<String, Integer> examples = new HashMap<>();
		examples.put("2 + 2 - 1", 3);
		examples.put("2 + 3 * 4", 14);
		examples.put("20 - 6 / 2", 17);
		examples.put("2 * 3 + 4 * 5", 26);
		examples.put("2 + 0 * 4 ", 2);

		for (Map.Entry<String, Integer> entry : examples.entrySet()) {
			String expr = entry.getKey();
			List<Token> tokens = tokenize(expr);
			Parser parser = new Parser(tokens);
			System.out.println(expr + " = " + parser.parseExpression() + " / Expected: " + entry.getValue());
		}
	}
}
