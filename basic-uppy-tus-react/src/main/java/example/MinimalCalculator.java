package example;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

// minimal, zero-dependency calculator parser in Java
// using a simple Recursive Descent Parser to handle operator precedence (like \(+\), \(-\)) and left-associativity

public class MinimalCalculator {

    enum Type { NUMBER, PLUS, MINUS, EOF }

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
        Pattern pattern = Pattern.compile("(\\s+)|([0-9]+)|(\\+)|(-)");
        Matcher matcher = pattern.matcher(input);

        while (matcher.find()) {
            if (
			matcher.group(1) != null) continue; // skip spaces
            if (matcher.group(2) != null) tokens.add(new Token(Type.NUMBER, matcher.group(2)));
            if (matcher.group(3) != null) tokens.add(new Token(Type.PLUS, matcher.group(3)));
            if (matcher.group(4) != null) tokens.add(new Token(Type.MINUS, matcher.group(4)));
        }
        tokens.add(new Token(Type.EOF, ""));
        return tokens;
    }

    static class Parser {
        List<Token> tokens;
        int pos = 0;

        Parser(List<Token> tokens) { this.tokens = tokens; }

        Token peek() { return tokens.get(pos); }
        Token consume() { return tokens.get(pos++); }

        // Primary -> NUMBER
        public int parsePrimary() {
            Token token = consume();
            return Integer.parseInt(token.text);
        }

        // Expression -> Primary ( (PLUS|MINUS) Primary )*
        public int parseExpression() {
            int result = parsePrimary();

            while (peek().type == Type.PLUS || peek().type == Type.MINUS) {
                Token op = consume();
                int next = parsePrimary();
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
        String expr = "2 + 2 - 1";
        List<Token> tokens = tokenize(expr);
        Parser parser = new Parser(tokens);
        System.out.println(expr + " = " + parser.parseExpression()); // Output: 3
    }
}

