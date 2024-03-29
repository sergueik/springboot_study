package example.repository;

import org.springframework.data.repository.reactive.ReactiveSortingRepository;
import org.springframework.stereotype.Repository;

import example.model.ItemTag;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Repository
public interface ItemTagRepository
		extends ReactiveSortingRepository<ItemTag, Long> {

	Flux<ItemTag> findAllByItemId(Long itemId);

	Mono<Integer> deleteAllByItemId(Long itemId);

}
