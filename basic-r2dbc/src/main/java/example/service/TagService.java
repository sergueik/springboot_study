package example.service;

import example.exception.TagNotFoundException;
import example.model.Tag;
import example.repository.TagRepository;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
public class TagService {
	private static final Sort DEFAULT_SORT = Sort.by(Sort.Order.by("name"));
	private final TagRepository tagRepository;

	public Flux<Tag> findAll() {
		return tagRepository.findAll(DEFAULT_SORT);
	}

	/**
	 * Find a Tag
	 *
	 * @param id      identifier of the tag
	 * @return the person
	 */
	public Mono<Tag> findById(final Long id) {
		return tagRepository.findById(id)
				.switchIfEmpty(Mono.error(new TagNotFoundException(id)));
	}

	@java.lang.SuppressWarnings("all")
	public TagService(final TagRepository tagRepository) {
		this.tagRepository = tagRepository;
	}
}
