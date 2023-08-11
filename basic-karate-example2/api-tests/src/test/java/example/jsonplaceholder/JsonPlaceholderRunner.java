package example.jsonplaceholder;

import org.junit.jupiter.api.Disabled;

import com.intuit.karate.junit5.Karate;

class JsonPlaceholderRunner {

	// NOTE: @Disabled has no effect with Karate
	@Disabled
	@Karate.Test
	Karate testUsers() {
		return Karate.run("jsonplaceholder").relativeTo(getClass());
	}

}
