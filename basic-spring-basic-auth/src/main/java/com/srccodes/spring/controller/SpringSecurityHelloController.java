package com.srccodes.spring.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

/**
 * @author Abhijit Ghosh
 * @version 1.0
 */

@Controller
public class SpringSecurityHelloController {

	@RequestMapping("/public")
	public String accessPublicPage(Model model) {
		model.addAttribute("message", "This page is publicly accessible. No authentication is required to view.");

		return "public";
	}
	
	@RequestMapping("/secured/mypage")
	public String accessSecuredPage(Model model) {
		model.addAttribute("message", "Only you are authenticated and authorized to view this page.");

		return "/secured/mypage";
	}
}
