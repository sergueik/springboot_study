package example.publicevents;

import com.intuit.karate.junit5.Karate;

class PubliceventsRunner {

	@Karate.Test
	Karate testUsers() {
		return Karate.run("token").relativeTo(getClass());
	}

}
