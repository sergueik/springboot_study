package example.controller;

import example.model.BackendData;
import example.service.RestService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Controller
@RequestMapping("/rest")
public class RestController {
	@Autowired
	RestService restService;

	@GetMapping("/{id}")
	public ResponseEntity<BackendData> getById(@PathVariable("id") int id) {
		BackendData data = restService.getBackendDataById(id);
		return new ResponseEntity<BackendData>(data, HttpStatus.OK);
	}

	@GetMapping
	public ResponseEntity<List<BackendData>> getAll() {
		List<BackendData> listRest = restService.getAll();
		return new ResponseEntity<List<BackendData>>(listRest, HttpStatus.OK);
	}

	@PostMapping
	public ResponseEntity<BackendData> addRest(@RequestBody BackendData data) {
		restService.addBackendData(data);
		BackendData data2 = restService.getBackendDataById(restService.latestInput());
		return new ResponseEntity<BackendData>(data2, HttpStatus.OK);
	}

	@PutMapping("/{id}")
	public ResponseEntity<BackendData> updateRest(@PathVariable("id") int id, @RequestBody BackendData data) {
		restService.updateBackendData(data, id);
		BackendData updatedData = restService.getBackendDataById(id);
		return new ResponseEntity<BackendData>(updatedData, HttpStatus.OK);
	}

	@DeleteMapping("/{id}")
	public ResponseEntity<BackendData> deleteRest(@PathVariable("id") int id) {
		BackendData data = restService.getBackendDataById(id);
		restService.deleteBackendDataById(id);
		return new ResponseEntity<BackendData>(data, HttpStatus.OK);
	}
}