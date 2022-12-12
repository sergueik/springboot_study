package com.pluralsight.data.input.validation.medicalfacility.controller;

import com.pluralsight.data.input.validation.medicalfacility.dto.PatientRequest;
import com.pluralsight.data.input.validation.medicalfacility.dto.PatientResponse;
import com.pluralsight.data.input.validation.medicalfacility.service.PatientsService;

import org.apache.commons.validator.routines.EmailValidator;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/patients")
public class PatientsController {

    private PatientsService patientsService;

    public PatientsController(PatientsService patientsService) {
        this.patientsService = patientsService;
    }

    @GetMapping
    public List<PatientResponse> getAllPatients() {
        return patientsService.getAllPatients();
    }

    @GetMapping("/{id}")
    public List<PatientResponse> getOnePatient(@RequestParam("id") Long id) {
        return patientsService.getAllPatients();
    }

    @PostMapping
    public PatientResponse createPatient(@RequestBody PatientRequest patientRequest) {
        validatePatient(patientRequest);
        return patientsService.createPatient(patientRequest);
    }

    private void validatePatient(PatientRequest patientRequest) {
        if(patientRequest.getFirstName().isBlank()) {
            throw new IllegalArgumentException("First name not provided");
        }
        if(patientRequest.getAge() < 0 || patientRequest.getAge() > 150) {
            throw new IllegalArgumentException("Age is incorrect");
        }
        if(!EmailValidator.getInstance()
                          .isValid(patientRequest.getEmail())) {
            throw new IllegalArgumentException("Email is invalid");
        }
    }
}
