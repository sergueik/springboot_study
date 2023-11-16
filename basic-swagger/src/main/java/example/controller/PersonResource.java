package example.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import example.domain.Person;

@RestController
@RequestMapping("/rest/person")
@Api(value = "person", description = "Shows the user info")

public class PersonResource {

	@GetMapping("/{userName}")
	@ApiOperation(value = "Get a Person object by userName", response = Person.class)
	public Person getPerson(@PathVariable("userName") final String userName) {
		return new Person(userName, "agent", 1000L);
	}
}
