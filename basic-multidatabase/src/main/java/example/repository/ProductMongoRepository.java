package example.repository;

import org.springframework.data.mongodb.repository.MongoRepository;

import example.model.MongoProduct;

public interface ProductMongoRepository
		extends MongoRepository<MongoProduct, Integer> {

}
