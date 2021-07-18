package example.controller;

import example.dao.UserDetailsServiceDAO;
import example.entity.User;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class UserController {

	@Autowired
	private UserDetailsServiceDAO userDetailsServiceDAO;

	@PreAuthorize("isAnonymous()")
	@RequestMapping(value = "/registration", method = RequestMethod.POST)
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

	@PreAuthorize("isAnonymous()")
	@RequestMapping(value = "/login", method = RequestMethod.GET)
	public String loginPage() {
		return "login";
	}

	@RequestMapping(value = "/profile", method = RequestMethod.GET)
	public String managePage() {
		return "profile";
	}

	@RequestMapping(value = "/profile", method = RequestMethod.POST)
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
