package com.pluralsight.data.input.validation.medicalfacility.repository;

import com.pluralsight.data.input.validation.medicalfacility.model.Patient;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PatientsRepository extends CrudRepository<Patient, Long> {

    List<Patient> findAll();
}
