package example.controller;

/**
 * Copyright 2022-2023 Serguei Kouzmine
 */

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.SessionAttributes;
import org.springframework.web.servlet.ModelAndView;


import example.utils.Utils;

import com.google.gson.FieldNamingStrategy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

@Controller
@SessionAttributes({ "custom" })
@RequestMapping("/thymeleaf")
public class ThymeleafController {

	private static final Logger logger = LoggerFactory
			.getLogger(ThymeleafController.class);
	private static Gson gson = new Gson();

	// NOTE: unreliable: only sets the "custom" attribute once
	@SuppressWarnings("deprecation")
	@ResponseBody
	@GetMapping("/custom")
	public String custom(HttpSession session,
			@RequestParam(value = "data", required = false, defaultValue = "data") String data) {
		String previousResult = (session.getAttribute("custom") != null)
				? session.getAttribute("custom").toString() : "";
		session.setAttribute("custom", data);
		String newResult = session.getAttribute("custom").toString();
		logger.info("Reading session attribute: " + newResult);
		Map<String, Object> info = new HashMap<>();
		info.put("new", newResult);
		info.put("old", previousResult);
		String responseBody = gson.toJson(info);
		logger.info("responseBody:\n" + responseBody);
		return responseBody;
	}

	// based on:
	// https://stackoverflow.com/questions/31387526/list-all-available-model-attributes-in-thymeleaf
	// see also:
	// https://www.thymeleaf.org/doc/articles/springmvcaccessdata.html
	// https://www.baeldung.com/spring-mvc-session-attributes
	// see also:
	// https://stackoverflow.com/questions/18791645/how-to-use-session-attributes-in-spring-mvc
	// TODO: see test
	// https://github.com/spring-projects/spring-framework/blob/main/spring-test/src/test/java/org/springframework/test/web/servlet/samples/standalone/resultmatchers/SessionAttributeAssertionTests.java
	@GetMapping("/debug")
	public ModelAndView debug_info() {

		ModelAndView model = new ModelAndView("/debug_info.html");
		model.addObject("name", "");
		model.addObject("console", Utils.getFileContent("dummy.txt"));
		return model;
	}
}
