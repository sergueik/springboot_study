package example.controller;

import example.dao.UserDetailsServiceDAO;
import example.entity.User;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
@RequestMapping(value = "/profile")
public class ProfileController {

	@Autowired
	private UserDetailsServiceDAO userDetailsServiceDAO;

	@GetMapping
	public String managePage() {
		return "profile";
	}

	@PostMapping
	public String manage(@RequestParam String username,
			@RequestParam String password, @RequestParam("new") String newPassord) {
		try {
			if (userDetailsServiceDAO.loadUserEntity(username, password)) {
				userDetailsServiceDAO.updateUser(new User(username, newPassord));
				return "redirect:" + "/profile?success";
			} else {
				return "redirect:" + "/profile?error";
			}
		} catch (Exception e) {
			e.printStackTrace();
			return "redirect:" + "/profile?errorServer";
		}
	}

}
