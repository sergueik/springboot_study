package example.controller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

import example.domain.Gender;
import example.domain.User;
import example.repository.UserRepository;

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

	private Map<Long, User> cachedUsers = new HashMap<>();

	@GetMapping("/getCachedUsers")
	public Map<Long, User> getCachedUsers() {
		if (cachedUsers.size() == 0) {
			List<User> users = userRepository.findAll();
			for (User user : users) {
				cachedUsers.put(user.getId(), user);
			}
		}
		return cachedUsers;
	}

	@RequestMapping("/getUser")
	public User getUser(Long id) {
		return userRepository.findOne(id);
	}

	@RequestMapping("/getCachedUser")
	public User getCachedUser(Long id) {
		if (cachedUsers.size() == 0) {
			cachedUsers = getCachedUsers();
		}
		return cachedUsers.containsKey(id) ? cachedUsers.get(id) : new User();
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
