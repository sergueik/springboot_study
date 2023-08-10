package example.events;

import com.intuit.karate.junit5.Karate;

class EventsRunner {

	@Karate.Test
	Karate testUsers() {
		return Karate.run("events").relativeTo(getClass());
	}

}
