package example.controller;

import example.model.BackendData;
import example.service.RestService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

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
		BackendData data2 = restService
				.getBackendDataById(restService.latestInput());
		return new ResponseEntity<BackendData>(data2, HttpStatus.OK);
	}

	@PutMapping("/{id}")
	public ResponseEntity<BackendData> updateRest(@PathVariable("id") int id,
			@RequestBody BackendData data) {
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

	@GetMapping(value = "/queryparam", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> queryParam(
			@RequestParam Optional<List<String>> keys,
			@RequestParam Optional<List<Integer>> ids) {
		String payload = null;
		if ((keys.isPresent() && keys.get().size() > 0)
				&& (ids.isPresent() && ids.get().size() > 0)) {
			payload = String.format("keys: %s ids: %s", String.join(",", keys.get()),
					String.join(",", ids.get().stream().map(o -> String.format("%d", o))
							.collect(Collectors.toList())));
			List<BackendData> listData = restService.queryByIds(ids.get());
			System.err.println("query by ids returned: " + listData.size() + " rows");
			listData = restService.queryByIdsAndKeys(ids.get(), keys.get());
			System.err.println("query by ids and keys returned: " + listData.size() + " rows");
			return ResponseEntity.status(HttpStatus.OK).body(payload);
		} else {
			return ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED).body("");
		}
	}

}
