package example.repository;

import org.springframework.data.jpa.repository.JpaRepository;


import example.domain.*;

public interface UserRepository extends JpaRepository<User, Long> {

}
