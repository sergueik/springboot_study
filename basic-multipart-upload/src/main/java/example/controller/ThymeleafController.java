package example.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;

@Controller
@RequestMapping("/upload")
public class ThymeleafController {

	@GetMapping
	public ModelAndView hello() {
		ModelAndView model = new ModelAndView("/hello.html");
		// model.addObject("console", Utils.listDirecroryFiles() );
		return model;
	}

}
