package example;

import org.springframework.stereotype.Component;


@Component
public class Service {

	public String hello() {
		return "Hello service";
	}
}