package example.exceptions;

import java.util.List;
import example.handler.dto.BusinessError;

@SuppressWarnings("serial")
public class BusinessRuleViolationException extends RuntimeException {

	private final List<BusinessError> errors;

	public BusinessRuleViolationException(List<BusinessError> errors) {
		super("Several business rules violated");
		this.errors = errors;
	}

	// If we find multiple errors, and want to return all violations to the client
	// rather than falling into a chatty type of engagement
	// we can leverage the errors array extension to include details
	// on all applicable errors for the associated problem type
	public List<BusinessError> getErrors() {
		return errors;
	}
}