package example.service;

import org.springframework.stereotype.Service;

@Service
// NOTE: should not use reserved name "Service" when naming the class :
// collision with stereotype:
// incompatible types: example.Service cannot be converted to
// java.lang.annotation.Annotation

public class ExampleService {

	public String hello() {
		final String hello = "Hello basic";
		System.err.println(hello);
		return hello;
	}
}