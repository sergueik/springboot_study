package example.rest;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = "/demo")
public class DemoResource {
	@RequestMapping(method = RequestMethod.GET)
	public String getDemo() {
		return "demo";
	}
}