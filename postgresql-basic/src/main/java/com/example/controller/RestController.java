package com.example.controller;

import com.example.model.Rest;
import com.example.model.RestResult;
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
	public ResponseEntity<RestResult> getById(@PathVariable("id") int id) {
		RestResult rest = restService.getRestById(id);
		return new ResponseEntity<RestResult>(rest, HttpStatus.OK);
	}

	@GetMapping
	public ResponseEntity<List<RestResult>> getAll() {
		List<RestResult> listRest = restService.getAll();
		return new ResponseEntity<List<RestResult>>(listRest, HttpStatus.OK);
	}

	@PostMapping
	public ResponseEntity<RestResult> addRest(@RequestBody Rest rest) {
		restService.addRest(rest);
		RestResult rest2 = restService.getRestById(restService.latestInput());
		return new ResponseEntity<RestResult>(rest2, HttpStatus.OK);
	}

	@PutMapping("/{id}")
	public ResponseEntity<RestResult> updateRest(@PathVariable("id") int id,
			@RequestBody Rest rest) {
		restService.updateRest(rest, id);
		RestResult rest2 = restService.getRestById(id);
		return new ResponseEntity<RestResult>(rest2, HttpStatus.OK);
	}

	@DeleteMapping("/{id}")
	public ResponseEntity<RestResult> deleteRest(@PathVariable("id") int id) {
		RestResult rest2 = restService.getRestById(id);
		restService.deleteRestById(id);
		return new ResponseEntity<RestResult>(rest2, HttpStatus.OK);
	}
}