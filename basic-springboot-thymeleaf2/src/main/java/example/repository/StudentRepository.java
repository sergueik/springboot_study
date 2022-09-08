package example.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import example.entity.Student;

@Repository
public interface StudentRepository extends JpaRepository<Student, Long> {
	List<Student> findByName(String name);
}
