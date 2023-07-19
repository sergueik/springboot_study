package example.repository;

import org.springframework.data.repository.reactive.ReactiveSortingRepository;
import org.springframework.stereotype.Repository;

import example.model.Item;

@Repository
public interface ItemRepository extends ReactiveSortingRepository<Item, Long> {

}
