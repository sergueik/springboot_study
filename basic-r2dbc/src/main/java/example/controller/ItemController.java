package example.controller;

import example.mapper.ItemMapper;
import example.rest.api.ItemPatchResource;
import example.rest.api.ItemResource;
import example.rest.api.ItemUpdateResource;
import example.rest.api.NewItemResource;
import example.rest.api.event.Event;
import example.rest.api.event.ItemDeleted;
import example.rest.api.event.ItemSaved;
import example.service.ItemService;
import io.swagger.annotations.ApiOperation;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.http.codec.ServerSentEvent;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.time.Duration;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;
import static org.springframework.http.MediaType.TEXT_EVENT_STREAM_VALUE;
import static org.springframework.http.ResponseEntity.created;
import static org.springframework.http.ResponseEntity.noContent;

@RestController
@RequestMapping("/items")
public class ItemController {
	@java.lang.SuppressWarnings("all")
	private static final org.slf4j.Logger log = org.slf4j.LoggerFactory
			.getLogger(ItemController.class);
	private final ItemService itemService;
	private final ItemMapper itemMapper;

	@ApiOperation("Create a new item")
	@PostMapping
	public Mono<ResponseEntity<Void>> create(
			@Valid @RequestBody final NewItemResource newItemResource) {
		return itemService.create(itemMapper.toModel(newItemResource))
				.map(item -> created(
						linkTo(ItemController.class).slash(item.getId()).toUri()).build());
	}

	@ApiOperation("Update an existing item")
	@PutMapping("/{id}")
	public Mono<ResponseEntity<Void>> update(@PathVariable @NotNull final Long id,
			@RequestHeader(HttpHeaders.IF_MATCH) final Long version,
			@Valid @RequestBody final ItemUpdateResource itemUpdateResource) {
		// Find the item and update the instance
		return itemService.findById(id, version, false)
				.map(item -> itemMapper.update(itemUpdateResource, item))
				.flatMap(itemService::update).map(item -> noContent().build());
	}

	@ApiOperation("Patch an existing item following the patch merge RCF (https://tools.ietf.org/html/rfc7396)")
	@PatchMapping("/{id}")
	public Mono<ResponseEntity<Void>> patch(@PathVariable @NotNull final Long id,
			@RequestHeader(HttpHeaders.IF_MATCH) final Long version,
			@Valid @RequestBody final ItemPatchResource patch) {
		return itemService.findById(id, version, true)
				.map(item -> itemMapper.patch(patch, item)).flatMap(itemService::update)
				.map(itemId -> noContent().build());
	}

	@ApiOperation("Find an item by its id")
	@GetMapping(value = "/{id}", produces = { APPLICATION_JSON_VALUE })
	public Mono<ItemResource> findById(@PathVariable final Long id) {
		return itemService.findById(id, null, true).map(itemMapper::toResource);
	}

	@ApiOperation("Get the list of items")
	@GetMapping(produces = TEXT_EVENT_STREAM_VALUE)
	public Flux<ItemResource> getAllItems() {
		return itemService.findAll().map(itemMapper::toResource);
	}

	@ApiOperation("Delete an item")
	@DeleteMapping("/{id}")
	public Mono<ResponseEntity<Void>> delete(@PathVariable final Long id,
			@RequestHeader(HttpHeaders.IF_MATCH) final Long version) {
		return itemService.deleteById(id, version)
				.map(empty -> noContent().build());
	}

	@GetMapping("/events")
	public Flux<ServerSentEvent<Event>> listenToEvents() {
		final Flux<Event> itemSavedFlux = this.itemService.listenToSavedItems()
				.map(itemMapper::toResource).map(ItemSaved::new);
		final Flux<Event> itemDeletedFlux = this.itemService.listenToDeletedItems()
				.map(ItemDeleted::new);
		return Flux.merge(itemSavedFlux, itemDeletedFlux)
				.map(event -> ServerSentEvent.<Event> builder()
						.retry(Duration.ofSeconds(4L))
						.event(event.getClass().getSimpleName()).data(event).build());
	}

	@java.lang.SuppressWarnings("all")
	public ItemController(final ItemService itemService,
			final ItemMapper itemMapper) {
		this.itemService = itemService;
		this.itemMapper = itemMapper;
	}
}
