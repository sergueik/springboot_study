package example.dao.user;

import org.springframework.data.jpa.repository.JpaRepository;

import example.model.user.User;

public interface UserRepository extends JpaRepository<User, Integer> {
}
