package example.controller;

/**
 * Copyright 2022-2023 Serguei Kouzmine
 */

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.SessionAttributes;
import org.springframework.web.servlet.ModelAndView;

import example.model.Person;

import example.utils.Utils;

@Controller
@SessionAttributes({ "custom" })
@RequestMapping("/thymeleaf")
public class ThymeleafController {

	private static final Logger logger = LoggerFactory
			.getLogger(ThymeleafController.class);

	@GetMapping
	public ModelAndView hello(
			@RequestParam(value = "name", required = false, defaultValue = "World") String name) {
		ModelAndView model = new ModelAndView("/hello.html");
		List<Person> persons = Arrays.asList(new Person(1, "A", "ford1"),
				new Person(2, "A", "ford2"), new Person(1, "B", "toyota1"),
				new Person(2, "B", "toyota2"));
		Map<Integer, List<Person>> groupedByIdPersons = persons.stream()
				.collect(Collectors.groupingBy(Person::getId));
		model.addObject("groupedByIdPersons", groupedByIdPersons);
		model.addObject("name", name);
		model.addObject("persons", persons);
		model.addObject("console", Utils.getFileContent("dummy.txt"));

		return model;
	}

	@GetMapping("/misconfigured")
	public ModelAndView misconfigured(
			@RequestParam(value = "name", required = false, defaultValue = "World") String name) {
		ModelAndView model = new ModelAndView("/hello");
		model.addObject("name", name);
		return model;
	}

	// NOTE: unreliable: only sets the "custom" attribute once
	@SuppressWarnings("deprecation")
	@ResponseBody
	@GetMapping("/custom")
	public String custom(HttpServletRequest request,
			@RequestParam(value = "data", required = false, defaultValue = "data") String data) {
		String previousResult = (request.getSession()
				.getAttribute("custom") != null)
						? request.getSession().getAttribute("custom").toString() : "";
		logger.info("Reading session attribute: " + previousResult);
		logger.info("Clearing session attribute.");
		// request.getSession().removeValue("custom");
		request.getSession().removeAttribute("custom");
		String newResult = (request.getSession().getAttribute("custom") != null)
				? request.getSession().getAttribute("custom").toString() : "";
		logger.info("Reading session attribute (after cleared): " + newResult);
		logger.info("Updating session attribute: " + data);
		request.getSession().setAttribute("custom", data);
		logger.info("Reading session attribute: "
				+ request.getSession().getAttribute("custom").toString());
		return previousResult;
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
	public ModelAndView debug_info(HttpServletRequest request) {
		// request.getSession().setAttribute("custom", "test");

		ModelAndView model = new ModelAndView("/debug_info.html");
		model.addObject("name", "");
		model.addObject("console", Utils.getFileContent("dummy.txt"));
		return model;
	}
}
