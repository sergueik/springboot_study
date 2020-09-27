package example.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import example.User;

public interface UserRepository extends JpaRepository<User, Long> {

}
