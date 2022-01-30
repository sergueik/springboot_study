package example.controller;

import java.util.stream.Collectors;

import javax.validation.Valid;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.Errors;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import example.model.Response;
import example.model.User;

@RestController
public class FieldValidationController {

	@PostMapping("/user")
	public ResponseEntity<?> process(@Valid @RequestBody User user,
			Errors errors) {
		Response response = new Response();
		// If error, just return a 400 bad request, along with the error message
		if (errors.hasErrors()) {
			// get all errors
			response.setMessage(errors.getAllErrors().stream()
					.map(x -> x.getDefaultMessage()).collect(Collectors.joining(",")));
		} else {
			response.setMessage("User inserted with Name " + user.getName());
		}
		return ResponseEntity.badRequest().body(response);

	}
}
