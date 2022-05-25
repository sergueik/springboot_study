package example.controller;

import java.io.UnsupportedEncodingException;
import java.net.URI;

import javax.net.ssl.SSLException;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

@RestController
@RequestMapping(path = "/")
public class EmployeeController {
	@GetMapping(path = "employees", produces = "application/json")
	public ResponseEntity<Object> nulMethod() throws Exception {
		// throw new SSLException("dummy");
		// willlead to a 500 seen by test
		return ResponseEntity.status(HttpStatus.OK).body("{}");
	}
}
