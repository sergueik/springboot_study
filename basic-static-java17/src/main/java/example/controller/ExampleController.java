package example.controller;
/**
 * Copyright 2022 Serguei Kouzmine
 */

// or
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

@Controller
// origin: discussion https://qna.habr.com/q/1197170
public class ExampleController {

	private Log log = LogFactory.getLog(this.getClass());

	@GetMapping("/home")
	public ModelAndView getHomePage() {
		log.info("Setting home page");
		ModelAndView modelAndView = new ModelAndView("home");

		return modelAndView;
	}
	// if "home.html" is put in "src/main/resources/static"
	// instead of "src/main/resources/templates"
	// error is
	// 2022-09-05 23:40:24.074 ERROR 4404 --- [nio-8080-exec-5]
	// o.a.c.c.C.[.[.[/].[disptcherServlet] : Servlet.service() for servlet
	// [dispatcherServlet] in context with path [] threw exception [Request
	// processing failed; nested exception is
	// org.thymeleaf.exceptions.TemplateInputException: Error resolving template
	// "home", template might not exist or might not be accessible by any of the
	// configured Template Resolvers]

	/*
	@GetMapping("/")
	public String getHomePage() {
		log.info("Setting home page");
		return "home";
	}
	*/
	@ResponseBody
	@GetMapping("/page")
	public String getPage() {
		return "page is here";
	}
}
