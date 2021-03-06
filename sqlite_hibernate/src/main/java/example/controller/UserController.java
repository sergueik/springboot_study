package example.controller;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import example.repository.UserRepository;

import example.data.User;
import example.data.Gender;

@RestController
public class UserController {

	private static final Logger LOGGER = LoggerFactory
			.getLogger(UserController.class);

	@Autowired
	private UserRepository userRepository;

	@GetMapping("/getUsers")
	public List<User> getUsers() {
		return userRepository.findAll();
	}

	@RequestMapping("/getUser")
	public User getUser(Long id) {
		return userRepository.findOne(id);
	}

	@PostMapping("/updateUser")
	public User updateUser(@RequestBody User targetUser) {
		User user = userRepository.findOne(targetUser.getId());
		if (targetUser.getUserName() != null) {
			user.setUserName(targetUser.getUserName());
		}
		if (targetUser.getPassword() != null) {
			user.setPassword(targetUser.getPassword());
		}
		if (targetUser.getNickName() != null) {
			user.setNickName(targetUser.getNickName());
		}
		if (targetUser.getGender() != null) {
			user.setGender(targetUser.getGender());
		}
		return userRepository.saveAndFlush(user);
	}

	@DeleteMapping("/deleteUser")
	public void deleteUser(Long id) {
		User user = userRepository.findOne(id);
		userRepository.delete(user);
	}

	// "Content-Type": "application/json"
	@PostMapping("/addUserObject")
	public User addUserObject(@RequestBody User newUser) {
		try {
			return userRepository.saveAndFlush(newUser);
		} catch (Exception e) {
			return newUser;
		}
	}

	// x-form-data
	@RequestMapping(value = "/addUser", method = RequestMethod.POST)
	@ResponseBody
	public String addUser(@RequestParam("userName") String userName,
			@RequestParam("password") String password,
			@RequestParam("confirmPassword") String confirmPassword,
			@RequestParam("gender") Gender gender,
			@RequestParam(name = "nickName", required = false) String nickName) {
		if (!(password.equals(confirmPassword))) {
			return "Password and confirmPassword do not match!";
		} else if (nickName != null && nickName.length() < 5) {
			return "nickName must be more 4 characters";
		} else {
			userRepository.save(new User(userName, password, gender));
			return "User added";
		}
	}
}
