package example;

import org.junit.Test;

import com.intuit.karate.KarateOptions;
import com.intuit.karate.Results;
import com.intuit.karate.Runner;

@KarateOptions(features = "classpath:")
public class KarateTest {

	@Test
	public void testSample() {
		Results results = Runner.parallel(getClass(), 1);
	}
}
