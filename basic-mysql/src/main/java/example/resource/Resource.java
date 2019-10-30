package example.resource;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/all")
public class Resource {

	private UserRepository usersRepository;

	public Resource(UserRepository usersRepository) {
		this.usersRepository = usersRepository;
	}

	@GetMapping("/")
	public List<Users> all() {

		return usersRepository.findAll();
	}
	// jist for sake of example all properties are hard coded
	@GetMapping("/create")
	public List<Users> users() {
		Users users = new Users();
		users.setId(1);
		users.setName("Bill");
		users.setSalary(30);
		users.setTeamName("Development");

		usersRepository.save(users);

		return usersRepository.findAll();
	}
}
