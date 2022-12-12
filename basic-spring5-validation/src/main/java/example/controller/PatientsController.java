package example.controller;

import example.dto.PatientRequest;
import example.dto.PatientResponse;
import example.service.PatientsService;

import org.apache.commons.lang3.StringUtils;
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
	public PatientResponse createPatient(
			@RequestBody PatientRequest patientRequest) {
		validatePatient(patientRequest);
		return patientsService.createPatient(patientRequest);
	}

	private void validatePatient(PatientRequest patientRequest) {

		// NOTE: isBlank requires Java 11
		// see also:
		// https://stackoverflow.com/questions/59425453/replacement-for-java-11-method-string-isblank-in-java-8
		if (patientRequest.getFirstName().isEmpty()
				|| patientRequest.getFirstName().trim().equals("")
				|| StringUtils.isBlank(patientRequest.getFirstName())) {
			throw new IllegalArgumentException("First name not provided");
		}
		if (patientRequest.getAge() < 0 || patientRequest.getAge() > 150) {
			throw new IllegalArgumentException("Age is incorrect");
		}
		if (!EmailValidator.getInstance().isValid(patientRequest.getEmail())) {
			throw new IllegalArgumentException("Email is invalid");
		}
	}
}
