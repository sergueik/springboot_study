package example.controller;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

@Controller
public class ExampleController {

	private Log log = LogFactory.getLog(this.getClass());

	@GetMapping("/home")
	public ModelAndView getHomePage() {
		log.info("Setting home page");
		ModelAndView modelAndView = new ModelAndView("home");

		return modelAndView;
	}

	@GetMapping("/")
	public String getHomePageString() {
		log.info("Setting home page");
		return "home";
		// ~[spring-webmvc-5.2.9.RELEASE.jar:5.2.9.RELEASE]
		// javax.servlet.ServletException: Could not resolve view with name 'home'
		// in servlet with name 'dispatcherServlet'
		// at
		// org.springframework.web.servlet.DispatcherServlet.render(DispatcherServlet.java:1353)
		// at
		// org.springframework.web.servlet.DispatcherServlet.processDispatchResult(DispatcherServlet.java:1118)
		// at
		// org.springframework.web.servlet.DispatcherServlet.doDispatch(DispatcherServlet.java:1057)
		// at
		// org.springframework.web.servlet.DispatcherServlet.doService(DispatcherServlet.java:943)
		// at
		// org.springframework.web.servlet.FrameworkServlet.processRequest(FrameworkServlet.java:1006)
		// at
		// org.springframework.web.servlet.FrameworkServlet.doGet(FrameworkServlet.java:898)
		// at javax.servlet.http.HttpServlet.service(HttpServlet.java:626)
	}

	@ResponseBody
	@GetMapping("/page")
	public String getPage() {
		return "page is here";
	}
}
