package example.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import example.data.User;

// see also: https://www.baeldung.com/spring-data-query-by-example
// about the JpaRepository interface
public interface UserRepository extends JpaRepository<User, Long> {

}
