package example;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/basic")
// .ConflictingBeanDefinitionException: Annotation-specified bean name 'worker'
// for bean class [example.Worker] conflicts with existing, non-compatible bean
// definition of same name and class [example.resource.Worker] -> [Help 1]
public class BasicWorker {

	@GetMapping
	public String Process() {
		return "Hello basic";
	}

}
