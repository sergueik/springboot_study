package example;

import com.intuit.karate.junit5.Karate;

public class DummyRunner {

    @Karate.Test
    Karate testDummy() {
        return Karate.run("classpath:features/dummy.feature");
    }
}
