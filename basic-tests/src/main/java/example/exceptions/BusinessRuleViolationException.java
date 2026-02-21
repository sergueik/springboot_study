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

	public List<BusinessError> getErrors() {
		return errors;
	}
}