package example.repository;

import java.math.BigInteger;
import java.util.List;
import java.util.Optional;

import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.stereotype.Repository;

import example.model.Person;

@Repository
// to avoid defining custom Convertors
// and not hit the exception
// No converter found capable of converting from [org.bson.types.ObjectId] to
// type [java.lang.Integer]
// when Long or Integer is used for id in example.model.Person
public interface PersonRepository extends MongoRepository<Person, BigInteger> {
	List<Person> findByNameLike(String nameRegex);

	@Query("{\"_id\": {$gte: ?0}}")
	List<Person> findByNumberGreaterOrEqualCustomQuery(BigInteger num);
}
