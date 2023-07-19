package example.repository;

import org.springframework.data.repository.reactive.ReactiveSortingRepository;
import org.springframework.stereotype.Repository;

import example.model.Person;

@Repository
public interface PersonRepository
		extends ReactiveSortingRepository<Person, Long> {

}
