package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import example.dao.UserDetailsServiceDAO;

@Controller
@RequestMapping(value = "/login")
public class LoginController {

	@Autowired
	private UserDetailsServiceDAO userDetailsServiceDAO;

	@PreAuthorize("isAnonymous()")
	@GetMapping
	public String loginPage() {
		return "login";
	}
}
