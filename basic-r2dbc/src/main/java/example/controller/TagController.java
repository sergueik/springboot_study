package example.controller;

import example.mapper.TagMapper;
import example.rest.api.TagResource;
import example.service.TagService;
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
@RequestMapping("/tags")
public class TagController {
	@java.lang.SuppressWarnings("all")
	private static final org.slf4j.Logger log = org.slf4j.LoggerFactory
			.getLogger(TagController.class);
	private final TagService tagService;
	private final TagMapper tagMapper;

	@ApiOperation("Find a tag by its id")
	@GetMapping(value = "/{id}", produces = { APPLICATION_JSON_VALUE })
	public Mono<TagResource> findById(@PathVariable final Long id) {
		return tagService.findById(id).map(tagMapper::toResource);
	}

	@ApiOperation("Get the tags")
	@GetMapping(produces = TEXT_EVENT_STREAM_VALUE)
	public Flux<TagResource> getAll() {
		return tagService.findAll().map(tagMapper::toResource);
	}

	@java.lang.SuppressWarnings("all")
	public TagController(final TagService tagService, final TagMapper tagMapper) {
		this.tagService = tagService;
		this.tagMapper = tagMapper;
	}
}
