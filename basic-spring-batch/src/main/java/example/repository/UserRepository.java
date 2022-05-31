package example.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import example.model.User;

public interface UserRepository extends JpaRepository<User, Integer> {
}
