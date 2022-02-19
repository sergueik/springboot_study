package example.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import example.model.Powerhouse;

import java.util.List;

public interface JpaPowerhouseRepository extends JpaRepository<Powerhouse, Integer> {

    @Query("SELECT o From Powerhouse o where o.userId=:id")
    List<Powerhouse> findByUserId(@Param("id") Integer i);

    @Query("SELECT o FROM Powerhouse o where o.userId = 0")
    List<Powerhouse> findAll();

    @Query("SELECT o FROM Powerhouse o where o.userId <> 0")
    List<Powerhouse> findAllUserPower();

    @Query("select p from Powerhouse p where p.level=:level1 and p.userId=:id")
    List<Powerhouse> findByLevel(@Param("level1") int level, @Param("id") int id);
}
