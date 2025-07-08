package example.repositories;

import org.springframework.data.jpa.repository.JpaRepository;

import example.entities.User;

public interface UserRepository extends JpaRepository<User, Long> {

}
