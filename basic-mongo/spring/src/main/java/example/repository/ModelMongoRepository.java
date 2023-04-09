package example.repository;

import org.springframework.data.mongodb.repository.MongoRepository;
import example.model.Model;

public interface ModelMongoRepository extends MongoRepository<Model, Long> {

}