package example.resource;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/basic")
public class Worker {
	
	@GetMapping
	public String Hello() {
		return "Hello basic";
	}

}
