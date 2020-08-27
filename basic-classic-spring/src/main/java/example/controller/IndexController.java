package example.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

// origin: https://github.com/xvitcoder/spring-mvc-angularjs

@Controller
@RequestMapping("/")
public class IndexController {

	@RequestMapping
	public String getIndexPage() {
		return "index";
	}
}
