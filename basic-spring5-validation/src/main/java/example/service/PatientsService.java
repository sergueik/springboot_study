package example.service;

import example.dto.PatientRequest;
import example.dto.PatientResponse;
import example.model.Patient;
import example.repository.PatientsRepository;
import org.springframework.stereotype.Service;

import java.util.List;

import static example.dto.PatientResponse.fromEntity;
import static java.util.stream.Collectors.toList;

@Service
public class PatientsService {

	private PatientsRepository patientsRepository;

	public PatientsService(PatientsRepository patientsRepository) {
		this.patientsRepository = patientsRepository;
	}

	public List<PatientResponse> getAllPatients() {
		return patientsRepository.findAll().stream()
				.map(patient -> fromEntity(patient)).collect(toList());
	}

	public PatientResponse getPatientById(Long id) {
		return patientsRepository.findById(id).map(patient -> fromEntity(patient))
				.orElseThrow(null);
	}

	public PatientResponse createPatient(PatientRequest patientRequest) {
		Patient patient = patientsRepository.save(patientRequest.toEntity());
		return fromEntity(patient);
	}
}
