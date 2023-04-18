package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import example.model.Person;
import example.service.PersonService;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("person")
public class PersonController {

	private final PersonService personService;

	public PersonController(PersonService personService) {
		this.personService = personService;
	}

	@PostMapping
	public Person create(@RequestBody Person person) {
		return personService.insertPersonData(person);
	}

	@GetMapping
	public Collection<Person> read() {
		return personService.getAllPersonInformation();
	}

	@GetMapping(path = "name/{name}")
	public List<Person> findByNameLike(@PathVariable("name") String name) {
		return personService.findByNameLike(name);
	}

	@GetMapping(path = "greater/{id}")
	public List<Person> findByNumberGreaterOrEqualCustomQuery(@PathVariable("id") Integer id) {
		return personService.findByNumberGreaterOrEqualCustomQuery(id);
	}

	@GetMapping(path = "{id}")
	public Optional<Person> readQueryUsingId(@PathVariable("id") Integer id) {
		return personService.getPersonInformationUsingId(id);
	}

	@PutMapping(path = "/update/{id}")
	public void update(@PathVariable Integer id, @RequestBody Person person) {
		personService.updatePersonUsingId(id, person);
	}

	@DeleteMapping(path = "/delete/{id}")
	public void delete(@PathVariable("id") Integer id) {
		personService.deletePersonUsingId(id);
	}
}
