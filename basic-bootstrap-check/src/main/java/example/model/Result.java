package example.model;

/**
 * Copyright 2023 Serguei Kouzmine
 */

import org.springframework.stereotype.Component;

@Component
public class Result {

	private String expression;
	private String exception;
	private String character;
	private String message;
	private int index = -1;

	public String getExpression() {
		return expression;
	}

	public void setExpression(String value) {
		expression = value;
	}

	public String getException() {
		return exception;
	}

	public void setException(String value) {
		exception = value;
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
			String exception) {
		this.expression = expression;
		this.character = character;
		this.message = message;
		this.exception = exception;
	}

	public Result(String expression, String character, String message,
			String exception, int index) {
		this.expression = expression;
		this.character = character;
		this.message = message;
		this.exception = exception;
		this.index = index;
	}

	public Result() {
	}
}

