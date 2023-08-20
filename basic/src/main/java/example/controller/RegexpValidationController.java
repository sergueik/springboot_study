package example.controller;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;

@RestController
@RequestMapping("/")
public class RegexpValidationController {

	private final Gson gson = new Gson();
	private Result result = new Result();
	private String payload = "";
	private boolean debug = false;

	public void setDebug(boolean data) {
		debug = data;
	}

	private final Logger log = LoggerFactory.getLogger(this.getClass());

	@ResponseBody
	@PostMapping(value = "/validate", consumes = {
			MediaType.APPLICATION_FORM_URLENCODED_VALUE }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<Map<String, Object>> validate(
			@RequestParam String expression, @RequestParam Optional<Boolean> fix) {
		return ResponseEntity.status(HttpStatus.OK)
				.body(processExpression(expression));
	}

	private Map<String, Object> processExpression(String expression) {
		Map<String, Object> response = new HashMap<>();
		log.info("Pattern expression {}", expression);
		try {
			Pattern p = Pattern.compile(expression, Pattern.MULTILINE);

			response.put("status", "OK");
			response.put("result", "");
			payload = gson.toJson(response);
			log.info("payload: {0}", payload);

		} catch (PatternSyntaxException e) {
			int index = e.getIndex();

			String formattedExpression1 = (index == 0 ? ""
					: expression.substring(0, index));

			String formattedExpression2 = expression.substring(index - 1, index);

			String formattedExpression3 = expression.substring(index);

			log.info("Exception: {} {} {} {}", e.toString(), formattedExpression1,
					formattedExpression2, formattedExpression3);

			result = new Result(expression, formattedExpression2, e.getMessage(),
					e.getClass().getName(), index);
			response.put("status", "error");
			response.put("result", result);
			payload = gson.toJson(response);
			log.info("payload: {0}", payload);

		} catch (Exception e) {
			log.info("Exception: {} ", e.toString());
			response.put("status", "error");
			response.put("result", e.toString());
		}
		return response;
	}

	public static class Result {

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

}
