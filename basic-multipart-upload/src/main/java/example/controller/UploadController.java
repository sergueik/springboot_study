package example.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
@RequestMapping("/")
public class UploadController {

	@GetMapping("/upload")
	public String display(@RequestParam(value = "message", required = false) String message, Model model) {
		return "upload";
	}

}
