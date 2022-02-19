package example.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import example.model.Action;

import java.util.List;

public interface JpaActionsRepository extends JpaRepository<Action, Integer> {

    @Query("SELECT a FROM Action a WHERE a.userId=:action")
    List<Action> findByUserId(@Param("action") int id);

    @Query("SELECT a FROM Action a WHERE a.userId=:action and a.nameCompany=:name")
    Action findByUserIdAndName(@Param("action") int id, @Param("name") String name);

}
