package example.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import example.model.User;

public interface UserRepository extends JpaRepository<User, Long> {
	User findByUsername(String username);
}
