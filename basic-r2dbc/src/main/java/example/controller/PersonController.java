package example.controller;

import example.mapper.PersonMapper;
import example.rest.api.PersonResource;
import example.service.PersonService;
import io.swagger.annotations.ApiOperation;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;
import static org.springframework.http.MediaType.TEXT_EVENT_STREAM_VALUE;

@RestController
@RequestMapping("/people")
public class PersonController {
	@java.lang.SuppressWarnings("all")
	private static final org.slf4j.Logger log = org.slf4j.LoggerFactory
			.getLogger(PersonController.class);
	private final PersonService personService;
	private final PersonMapper personMapper;

	@ApiOperation("Find a person by his id")
	@GetMapping(value = "/{id}", produces = { APPLICATION_JSON_VALUE })
	public Mono<PersonResource> findById(@PathVariable final Long id) {
		return personService.findById(id).map(personMapper::toResource);
	}

	@ApiOperation("Get the people")
	@GetMapping(produces = TEXT_EVENT_STREAM_VALUE)
	public Flux<PersonResource> getAll() {
		return personService.findAll().map(personMapper::toResource);
	}

	@java.lang.SuppressWarnings("all")
	public PersonController(final PersonService personService,
			final PersonMapper personMapper) {
		this.personService = personService;
		this.personMapper = personMapper;
	}
}
