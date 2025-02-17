package example;

import java.util.List;

import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

@RepositoryRestResource(collectionResourceRel = "people", path = "people")
public interface PersonRepository
		extends PagingAndSortingRepository<Person, Long> {

	List<Person> findByLastName(@Param("lastName") String lastName);

	List<Person> findByFirstName(@Param("firstName") String firstName);

}