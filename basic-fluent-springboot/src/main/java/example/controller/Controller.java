package example;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import java.util.*;


@RestController
public class Controller {

	@GetMapping("/hello")
	public String hello() {
		return "Hello from fluent app!";

	}
}
