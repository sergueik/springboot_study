package example.service;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.gson.Gson;

import example.model.Result;

@Component
public class ProcessExpressionService {
	private final Logger log = LoggerFactory.getLogger(this.getClass());
	@Autowired
	private Result result;
	@Autowired
	private Gson gson;

	private String payload = "";

	public Map<String, Object> processExpression(String expression) {
		Map<String, Object> response = new HashMap<>();
		log.info("Pattern expression {}", expression);
		try {
			Pattern p = Pattern.compile(expression, Pattern.MULTILINE);

			response.put("status", "OK");
			response.put("result", "");
			payload = gson.toJson(response);
			log.info("payload: {}", payload);

		} catch (PatternSyntaxException e) {
			int index = e.getIndex();

			String formattedExpression1 = (index == 0 ? ""
					: expression.substring(0, index));

			String formattedExpression2 = expression.substring(index - 1, index);

			String formattedExpression3 = expression.substring(index);

			log.info("Exception: {} {} {} {}", e.toString(), formattedExpression1,
					formattedExpression2, formattedExpression3);

			result.setExpression(expression);
			result.setMessage(e.getMessage());
			result.setCharacter(formattedExpression2);
			result.setException(e.getClass().getName());
			result.setIndex(index);

			response.put("status", "error");
			response.put("result", result);
			payload = gson.toJson(response);
			log.info("payload: {}", payload);

		} catch (Exception e) {
			log.info("Exception: {} ", e.toString());
			response.put("status", "error");
			response.put("result", e.toString());
		}
		return response;
	}

}
