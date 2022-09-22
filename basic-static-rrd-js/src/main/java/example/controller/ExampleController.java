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

	@GetMapping("/")
	public String getIndexPage() {
		log.info("Setting home page");
		return "home";
	}
}
