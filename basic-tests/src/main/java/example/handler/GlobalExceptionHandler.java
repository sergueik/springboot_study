package example.handler;

import example.exceptions.BusinessRuleViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
// the class ProblemDetail added in Spring Framework 6.0
// RFC 7807 (Problem Details for HTTP APIs) is from 2016
// import org.springframework.http.ProblemDetail;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

@RestControllerAdvice
public class GlobalExceptionHandler {
	/*
	 * { "type": "https://problems-registry.smartbear.com/business-rule-violation",
	 * "title": "Business rule violation", "status": 422, "detail":
	 * "Order quantity must not exceed 10 items", "rule": "MAX_ORDER_QUANTITY" }
	 */
	/*
	 * @ExceptionHandler(BusinessRuleViolationException.class) public ProblemDetail
	 * handleBusinessRuleViolation(BusinessRuleViolationException ex) {
	 * 
	 * ProblemDetail problem =
	 * ProblemDetail.forStatus(HttpStatus.UNPROCESSABLE_ENTITY);
	 * 
	 * problem.setType(URI.create(
	 * "https://problems-registry.smartbear.com/business-rule-violation"));
	 * problem.setTitle("Business rule violation");
	 * problem.setDetail(ex.getMessage());
	 * 
	 * problem.setProperty("rule", ex.getRule());
	 * 
	 * return problem; }
	 */
	public static final String BUSINESS_RULE_VIOLATION_TYPE = "https://problems-registry.smartbear.com/business-rule-violation";

	@ExceptionHandler(BusinessRuleViolationException.class)
	public ResponseEntity<Map<String, Object>> handleBusinessRuleViolation(BusinessRuleViolationException ex) {

		Map<String, Object> body = new HashMap<>();
		body.putAll(Map.of("type", BUSINESS_RULE_VIOLATION_TYPE, "title", "Business rule violation", "status",
				HttpStatus.UNPROCESSABLE_ENTITY.value(), "detail", ex.getMessage(), "rule", ex.getRule()));

		return ResponseEntity.unprocessableEntity().body(body);
	}
}