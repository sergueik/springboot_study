package example;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.Assume;
import com.google.gson.Gson;

import example.service.ExampleService;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

public class PropertyParserExceptionTest {

	private final Gson gson = new Gson();
	private Map<String, Object> response = new HashMap<>();
	private String expression = null;
	private Result result = new Result();
	private String payload = "";

	private Map<String, Integer> expressions = new HashMap<>();

	@Test
	public void test2() throws Exception {

		expressions.put("Lorem ipsum** dolor sit amet", 12);
		expressions.put("Lorem ipsum do??lor sit amet", -1);
		expressions.put("Lorem ipsum do[lor sit amet", 26);
		expressions.put("Lorem ipsum]] dolor sit amet", -1);
		expressions.put("Lorem ip\\sum dolor sit amet", -1);
		expressions.put("Lorem \\ipsum dolor sit amet", -1);
		expressions.put("Lorem \\Ipsum dolor sit amet", 7);
		expressions.put("\\Lorem ipsum dolor sit amet", 1);
		expressions.put("Lorem *? dolor sit amet", -1);
		expressions.put("Lorem \\\\ipsum dolor sit amet", -1);

		for (String expr : expressions.keySet()) {
			response = processExpression(expr);
			// assertThat();
		}
	}

	@Test
	public void test1() throws Exception {

		expression = "ab\\{cd**f\\}	gh\\I";
		System.err.println(String.format("Pattern exression %s:\n", expression));
		try {
			Pattern p = Pattern.compile(expression, Pattern.MULTILINE);
			// System.err.println("Pattern:\n" + p.toString());
			response.put("status", "OK");
			response.put("result", "");
			payload = gson.toJson(response);
			System.err.println("payload: " + payload);

		} catch (PatternSyntaxException e) {
			int index = e.getIndex();
			System.err.println(String.format("=> %d/%d", 0, index));
			String formattedExpression1 = (index == 0 ? ""
					: expression.substring(0, index));
			System.err.println(String.format("==> %d/%d", 0, index - 1, 1));

			String formattedExpression2 = expression.substring(index - 1, index);
			System.err.println(String.format("===> %d", index));

			String formattedExpression3 = expression.substring(index);

			System.err
					.println("Exception: " + e.toString() + " " + formattedExpression1
							+ " " + formattedExpression2 + " " + formattedExpression3);

			result = new Result(expression, formattedExpression2, e.toString(),
					index);
			response.put("status", "error");
			response.put("result", result);
			payload = gson.toJson(response);
			System.err.println("payload: " + payload);

		} catch (Exception e) {
			System.err.println("Exception: " + e.toString());
		}

	}

	private Map<String, Object> processExpression(String expression) {
		Map<String, Object> response = new HashMap<>();
		System.err.println(String.format("Pattern exression %s:\n", expression));
		try {
			Pattern p = Pattern.compile(expression, Pattern.MULTILINE);
			// System.err.println("Pattern:\n" + p.toString());
			response.put("status", "OK");
			response.put("result", "");
			payload = gson.toJson(response);
			System.err.println("payload: " + payload);

		} catch (PatternSyntaxException e) {
			int index = e.getIndex();
			System.err.println(String.format("=> %d/%d", 0, index));
			String formattedExpression1 = (index == 0 ? ""
					: expression.substring(0, index));
			System.err.println(String.format("==> %d/%d", 0, index - 1, 1));

			String formattedExpression2 = expression.substring(index - 1, index);
			System.err.println(String.format("===> %d", index));

			String formattedExpression3 = expression.substring(index);

			System.err
					.println("Exception: " + e.toString() + " " + formattedExpression1
							+ " " + formattedExpression2 + " " + formattedExpression3);

			result = new Result(expression, formattedExpression2, e.toString(),
					index);
			response.put("status", "error");
			response.put("result", result);
			payload = gson.toJson(response);
			System.err.println("payload: " + payload);

		} catch (Exception e) {
			System.err.println("Exception: " + e.toString());
			response.put("status", "error");
			response.put("result", e.toString());
		}
		return response;
	}

	public static class Result {

		private String expression;
		private String character;
		private String message;
		private int index = -1;

		public String getExpression() {
			return expression;
		}

		public void setExpression(String value) {
			expression = value;
		}

		public String getCharacter() {
			return character;
		}

		public void setCharacter(String value) {
			character = value;
		}

		public String getMessage() {
			return message;
		}

		public void setMessage(String value) {
			message = value;
		}

		public int getIndex() {
			return index;
		}

		public void setIndex(int value) {
			index = value;
		}

		public Result(String expression) {
			this.expression = expression;
		}

		public Result(String expression, String character) {
			this.expression = expression;
			this.character = character;
		}

		public Result(String expression, String character, String message) {
			this.expression = expression;
			this.character = character;
			this.message = message;
		}

		public Result(String expression, String character, String message,
				int index) {
			this.expression = expression;
			this.character = character;
			this.message = message;
			this.index = index;
		}

		public Result() {
		}
	}

}
