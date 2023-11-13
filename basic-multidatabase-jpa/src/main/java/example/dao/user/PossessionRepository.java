package example.dao.user;

import org.springframework.data.jpa.repository.JpaRepository;

import example.model.user.Possession;

public interface PossessionRepository extends JpaRepository<Possession, Long> {

}
