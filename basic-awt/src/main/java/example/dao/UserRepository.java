package example.dao;

import org.springframework.data.repository.CrudRepository;

import example.entity.User;

import java.util.Optional;

public interface UserRepository extends CrudRepository<User, Long> {

	Optional<User> findByEmail(String email);
}
