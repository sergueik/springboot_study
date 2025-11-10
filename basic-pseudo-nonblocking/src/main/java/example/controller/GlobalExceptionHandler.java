package example.controller;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.HashMap;
import java.util.Map;

@RestControllerAdvice
public class GlobalExceptionHandler {

	@SuppressWarnings("unchecked")
	@ExceptionHandler(MethodArgumentNotValidException.class)
	public ResponseEntity<Map<String, String>> handleValidationExceptions(MethodArgumentNotValidException e) {
		// NOTE: classic “pre-var habit” trap. 
		// cannot omit key, value types in the rhs of the var local parameter declaration leads compiler to assume 
		// HashMap<Object,Object> which is incompatible with ResponseEntity<Map<String,String>>:
		// Map<String,String> and Map<Object,Object> are not compatible
		// var errors = new HashMap<>();
		// lead to compilation error:
		// Type mismatch: cannot convert from ResponseEntity<HashMap<Object,Object>> to
		// ResponseEntity<Map<String,String>>
		Map errors = new HashMap<String, String>();
		e.getBindingResult().getFieldErrors().forEach(error -> errors.put(error.getField(), error.getDefaultMessage()));
		return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(errors);
	}
}
