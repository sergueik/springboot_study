package example.repository;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveSortingRepository;
import org.springframework.stereotype.Repository;

import example.model.Tag;
import reactor.core.publisher.Flux;

@Repository
public interface TagRepository extends ReactiveSortingRepository<Tag, Long> {

    @Query("select t.* from tag t join item_tag it on t.id = it.tag_id where it.item_id = :item_id order by t.name")
    Flux<Tag> findTagsByItemId(Long itemId);

}
