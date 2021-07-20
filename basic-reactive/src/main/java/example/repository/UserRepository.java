package example.repository;

import org.springframework.data.mongodb.repository.ReactiveMongoRepository;

import example.model.User;

public interface UserRepository extends ReactiveMongoRepository<User, String> {

}
