package example.resource;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;

@RestController
@RequestMapping("/all")
public class Resource {

	private UserRepository usersRepository;

	public Resource(UserRepository usersRepository) {
		this.usersRepository = usersRepository;
	}

	// handle errors with HTTP errors
	// https://www.baeldung.com/spring-response-status-exception
	@GetMapping("/users/{id}")
	public Users getUsers(@PathVariable("id") int id) {
		// https://github.com/spring-projects/spring-framework/blob/master/spring-web/src/main/java/org/springframework/http/HttpStatus.java
		System.err.println("in custom exception handler");
		/* need to be an error status */
		throw new ResponseStatusException(HttpStatus.NO_CONTENT, /* HttpStatus.NOT_FOUND HttpStatus.NO_CONTENT, */
				String.format("This is what actually happened with that id %d.", id), null);
	}

	@GetMapping("/user/{id}")
	public Users getOne(@PathVariable("id") int id) {
		try {
			return usersRepository.getOne(id);
		} catch (Exception e) {
			throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Id Not Found", e);
		}
	}

	@GetMapping("/")
	public List<Users> all() {
		return usersRepository.findAll();
	}

	// for the sake of example all properties are hard coded
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
