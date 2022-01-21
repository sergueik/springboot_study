package example;

import org.springframework.data.mongodb.repository.MongoRepository;
import example.Model;

public interface ModelMongoRepository extends MongoRepository<Model, Long> {

}