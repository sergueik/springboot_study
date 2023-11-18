package example;

import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.MeterRegistry;
import org.springframework.jmx.export.annotation.ManagedOperation;
import org.springframework.jmx.export.annotation.ManagedOperationParameters;
import org.springframework.jmx.export.annotation.ManagedResource;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController()
@RequestMapping("/mike/api")
@ManagedResource(objectName = "MikesMBeans:category=MBeans,name=testBean")
public class GreetingsController {
	final Counter mikes_greeting;

	public GreetingsController(MeterRegistry registry) {
		mikes_greeting = registry.counter("mikesgreeting");
	}

	@GetMapping("/greeting")
	@ManagedOperation
	public String greeting() throws InterruptedException {
		mikes_greeting.increment();
		return "hello... :-)";
	}

	@ManagedOperation
	@ManagedOperationParameters()
	public String greetingWithName(String name) throws InterruptedException {
		mikes_greeting.increment();
		return "hello... :-) " + name;
	}

	@ManagedOperation
	public String greetingsReturnCount() throws InterruptedException {
		mikes_greeting.increment();
		return "hello... :-) " + mikes_greeting.count();
	}

}
