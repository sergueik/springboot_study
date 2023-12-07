package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.List;

@Controller
@RequestMapping("/student")
public class StudentController {

	// 3. display data
	@GetMapping("/all")
	public String display(
			@RequestParam(value = "message", required = false) String message,
			Model model) {
		return "StudentData";
	}

	@GetMapping("/hello")
	public ModelAndView hello() {
		ModelAndView model = new ModelAndView("/StudentData.html");
		// model.addObject("console", Utils.listDirecroryFiles() );
		return model;
	}

}