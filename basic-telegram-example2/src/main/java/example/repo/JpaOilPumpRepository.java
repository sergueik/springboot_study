package example.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import example.model.OilPump;

import java.util.List;

public interface JpaOilPumpRepository extends JpaRepository<OilPump, Integer> {

    @Query("SELECT o From OilPump o where o.userId=:id")
    List<OilPump> findByUserId(@Param("id") Integer i);

    @Query("SELECT o FROM OilPump o where o.userId = 0")
    List<OilPump> findAll();

    @Query("SELECT o FROM OilPump o where o.userId <> 0")
    List<OilPump> findAllByUsersOil();

    @Query("select o from OilPump o where o.level=:level1 and o.userId=:id")
    List<OilPump> findByLevel(@Param("level1") int level, @Param("id") int id);
}
