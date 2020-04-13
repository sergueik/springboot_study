package com.example.controller;

import com.example.model.Rest;
import com.example.service.RestService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Controller
@RequestMapping("rest")
public class RestController {
	@Autowired
	RestService restService;

	@GetMapping("/{id}")
	public ResponseEntity<Rest> getById(@PathVariable("id") int id) {
		Rest rest = restService.getRestById(id);
		return new ResponseEntity<Rest>(rest, HttpStatus.OK);
	}

	@GetMapping
	public ResponseEntity<List<Rest>> getAll() {
		List<Rest> listRest = restService.getAll();
		return new ResponseEntity<List<Rest>>(listRest, HttpStatus.OK);
	}

	@PostMapping
	public ResponseEntity<Rest> addRest(@RequestBody Rest rest) {
		restService.addRest(rest);
		Rest rest2 = restService.getRestById(restService.latestInput());
		return new ResponseEntity<Rest>(rest2, HttpStatus.OK);
	}

	@PutMapping("/{id}")
	public ResponseEntity<Rest> updateRest(@PathVariable("id") int id, @RequestBody Rest rest) {
		restService.updateRest(rest, id);
		Rest rest2 = restService.getRestById(id);
		return new ResponseEntity<Rest>(rest2, HttpStatus.OK);
	}

	@DeleteMapping("/{id}")
	public ResponseEntity<Rest> deleteRest(@PathVariable("id") int id) {
		Rest rest2 = restService.getRestById(id);
		restService.deleteRestById(id);
		return new ResponseEntity<Rest>(rest2, HttpStatus.OK);
	}
}