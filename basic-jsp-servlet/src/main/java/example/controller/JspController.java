package example.controller;
/**
 * Copyright 2022-2023 Serguei Kouzmine
 */

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import example.utils.Utils;

@Controller
public class JspController {

	@RequestMapping("/jsp")
	public String jsp(Model model,
			@RequestParam(value = "name", required = false, defaultValue = "World") String name) {
		model.addAttribute("name", name);
		model.addAttribute("id", "0");
		model.addAttribute("console", Utils.getFileContent("dummy.txt"));
		return "/hello.jsp";
	}

	@RequestMapping("/upload")
	public String upload(Model model,
			@RequestParam(value = "name", required = false, defaultValue = "World") String name) {
		model.addAttribute("name", name);
		model.addAttribute("id", "0");
		model.addAttribute("console", Utils.getFileContent("dummy.txt"));
		return "/upload.jsp";
	}

}
