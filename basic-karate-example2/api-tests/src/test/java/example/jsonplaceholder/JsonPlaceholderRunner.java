package example.jsonplaceholder;

import com.intuit.karate.junit5.Karate;

class JsonPlaceholderRunner {

	@Karate.Test
	Karate testUsers() {
		return Karate.run("jsonplaceholder").relativeTo(getClass());
	}

}
