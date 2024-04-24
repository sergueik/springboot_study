package example.controller;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
@RequestMapping("/")
public class UploadController {

	@Value("${debug}")
	private boolean debug;

	@GetMapping("/upload")
	public String display(@RequestParam(value = "message", required = false) String message, Model model) {
		model.addAttribute("debug", debug);
		return "upload";
	}

}
