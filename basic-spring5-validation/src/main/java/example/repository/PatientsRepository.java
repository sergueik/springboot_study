package example.repository;

import example.model.Patient;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PatientsRepository extends CrudRepository<Patient, Long> {

    List<Patient> findAll();
}
