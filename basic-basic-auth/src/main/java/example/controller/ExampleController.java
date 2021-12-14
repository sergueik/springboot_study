package example.controller;

import java.io.UnsupportedEncodingException;
import java.net.URI;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import example.dao.EmployeeDAO;
import example.model.Employee;
import example.model.Employees;
import org.apache.commons.codec.binary.Base64;

@RestController
@RequestMapping(path = "/")
public class ExampleController {
	@Autowired
	private EmployeeDAO employeeDao;

	@GetMapping(path = "employees", produces = "application/json")
	public Employees getEmployees() {
		return employeeDao.getEmployees();
	}

	@PostMapping(path = "null")
	public ResponseEntity<Object> nulMethod() throws Exception {
		return ResponseEntity.status(HttpStatus.OK).body(null);
	}

	// based on:
	// https://www.java2novice.com/restful-web-services/http-basic-authentication/
	@GetMapping(path = "echo", produces = "text/plain")
	public ResponseEntity<Object> echoCredentials(
			@RequestHeader(name = "authorization", required = true) String authString) {
		String decoded = getCredentials(authString);
		if (decoded == null) {
			return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
					.body("cannot decode credentials");
		}
		return ResponseEntity.status(HttpStatus.OK).body(decoded);
	}

	private String getCredentials(String authString) {

		String decoded = "";
		String[] authParts = authString.split("\\s+");
		String authInfo = authParts[1];
		try {
			decoded = new String(Base64.decodeBase64(authInfo.getBytes("UTF8")));
			System.err.println(decoded);
		} catch (UnsupportedEncodingException e) {
			System.err.println("Exception (reported): " + e.toString());
			return null;
		}
		return decoded;
	}

	@PostMapping(path = "employees", consumes = "application/json", produces = "application/json")
	public ResponseEntity<Object> addEmployee(
			@RequestHeader(name = "X-COM-PERSIST", required = true) String headerPersist,
			@RequestHeader(name = "X-COM-LOCATION", defaultValue = "ASIA") String headerLocation,
			@RequestBody Employee employee) throws Exception {
		// Generate resource id
		Integer id = employeeDao.getEmployees().getEmployees().size() + 1;
		employee.setId(id);

		// add resource
		employeeDao.addEmployee(employee);

		// Create resource location
		URI location = ServletUriComponentsBuilder.fromCurrentRequest()
				.path("/{id}").buildAndExpand(employee.getId()).toUri();

		// Send location in response
		return ResponseEntity.created(location).build();
	}
}
