package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import example.model.Person;
import example.model.Ticket;
import example.service.PersonService;
import example.service.TicketService;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("person")
public class PersonController {

	@Autowired
	private final PersonService personService;
	@Autowired
	private final TicketService ticketService;

	public PersonController(PersonService personService,
			TicketService ticketService) {
		this.personService = personService;
		this.ticketService = ticketService;
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
	public List<Person> findByNumberGreaterOrEqualCustomQuery(
			@PathVariable("id") Integer id) {
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

	@PutMapping(path = "/addticket/{id}")
	public void addTickets(@PathVariable Integer id,
			@RequestBody List<Ticket> tickets) {
		personService.addTickets(id, tickets);
	}

	@DeleteMapping(path = "/delete/{id}")
	public void delete(@PathVariable("id") Integer id) {
		personService.deletePersonUsingId(id);
	}

	@RequestMapping(value = "/tickets/{id}", method = RequestMethod.GET)
	public Optional<Ticket> getTicketById(@PathVariable("id") String id) {
		return ticketService.getTicketById(id);
	}

	@RequestMapping(value = "/tickets", method = RequestMethod.POST)
	public Ticket addNewApplication(@RequestBody Ticket ticket) {
		return ticketService.addNewApplication(ticket);
	}

	@RequestMapping(value = "/tickets/{id}", method = RequestMethod.PUT)
	public Ticket updateApplication(@PathVariable("id") String id,
			@RequestBody Ticket ticket) {
		return ticketService.updateApplication(id, ticket);
	}

	@RequestMapping(value = "/tickets/{id}", method = RequestMethod.DELETE)
	public void deleteTicket(@PathVariable("id") String id) {
		ticketService.deleteTicket(id);
	}

}
