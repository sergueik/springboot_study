package example.exceptionhandler;

import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import example.dto.ErrorResponse;
import static org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR;
import static org.springframework.http.HttpStatus.BAD_REQUEST;

@RestControllerAdvice("example")
public class GlobalExceptionHandler {

	@ExceptionHandler(IllegalArgumentException.class)
	@ResponseStatus(BAD_REQUEST)
	public ErrorResponse handleIllegalArgumentException(
			IllegalArgumentException exception) {
		return ErrorResponse.builder().message(exception.getMessage()).build();
	}

	@ExceptionHandler(Exception.class)
	@ResponseStatus(INTERNAL_SERVER_ERROR)
	public ErrorResponse handleAllExceptions(Exception exception) {
		return ErrorResponse.builder().message(exception.getMessage()).build();
	}
}
