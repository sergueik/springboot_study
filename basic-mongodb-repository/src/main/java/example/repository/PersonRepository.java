package example.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.stereotype.Repository;

import example.model.Person;

@Repository
public interface PersonRepository extends MongoRepository<Person, String> {
	List<Person> findByNameLike(String nameRegex);
	
	@Query("{\"id\": {$gte: ?0}}")
	List<Person> findByNumberGreaterOrEqualCustomQuery(int num);
}
