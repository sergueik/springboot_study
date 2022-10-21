package example.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;

@Controller
@RequestMapping("/thymeleaf")
public class ThymeleafController {

	@GetMapping
	public ModelAndView hello(
			@RequestParam(value = "name", required = false, defaultValue = "World") String name) {
		ModelAndView model = new ModelAndView("/hello.html");
		model.addObject("name", name);
		return model;
	}

	// javax.servlet.ServletException: Could not resolve view with name '/hello'
	// in servlet with name 'dispatcherServlet'
	// would also be thrown when resolvers are not configured properly
	@GetMapping("/misconfigured")
	public ModelAndView misconfigured(
			@RequestParam(value = "name", required = false, defaultValue = "World") String name) {
		ModelAndView model = new ModelAndView("/hello");
		model.addObject("name", name);
		return model;
	}
}
