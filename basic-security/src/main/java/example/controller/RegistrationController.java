package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import example.dao.UserDetailsServiceDAO;
import example.entity.User;

@Controller
@RequestMapping(value = "/registration")
public class RegistrationController {

	@Autowired
	private UserDetailsServiceDAO userDetailsServiceDAO;

	@PreAuthorize("isAnonymous()")
	@PostMapping
	public String registration(User newUser) {
		try {
			if (userDetailsServiceDAO
					.loadUserEntityByUsername(newUser.getUsername()) != null) {
				return "redirect:" + "/login?registration&error";
			} else {
				userDetailsServiceDAO.saveUser(newUser);
				return "redirect:" + "/login?registration&success";
			}
		} catch (Exception e) {
			e.printStackTrace();
			return "redirect:" + "/login?registration&errorServer";
		}
	}
}
