package example.exceptionHandler;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

@ControllerAdvice	
public class CustomExceptionHandler extends ResponseEntityExceptionHandler{
	
	@ExceptionHandler(Exception.class)
    public final ResponseEntity<ConversionExceptionDetailData> handleAllExceptions(ConversionException ex) {
        
        
		ConversionExceptionDetailData error = 
				new ConversionExceptionDetailData(HttpStatus.INTERNAL_SERVER_ERROR.value(),ex.getMessage()); 
        return new ResponseEntity(error, HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
