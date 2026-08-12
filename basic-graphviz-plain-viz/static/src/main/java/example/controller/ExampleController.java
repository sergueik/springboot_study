package example.controller;
/**
 * Copyright 2022,2026 Serguei Kouzmine
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

	@ResponseBody
	@GetMapping("/page")
	public String getPage() {
		return "page is here";
	}
}
