package example.controller;

/**
 * Copyright 2022-2023 Serguei Kouzmine
 */

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;

import example.model.Person;

import example.utils.Utils;

@Controller
@RequestMapping("/thymeleaf")
public class ThymeleafController {

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
}
