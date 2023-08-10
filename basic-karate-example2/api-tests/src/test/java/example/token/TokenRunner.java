package example.token;

import com.intuit.karate.junit5.Karate;

class TokenRunner {

	@Karate.Test
	Karate testUsers() {
		return Karate.run("token").relativeTo(getClass());
	}

}
