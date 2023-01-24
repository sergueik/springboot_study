package karate;

import com.intuit.karate.junit4.Karate;

import org.junit.Test;

import com.intuit.karate.KarateOptions;
import com.intuit.karate.Results;
import com.intuit.karate.Runner;

@KarateOptions(features = "classpath:")
public class TestRunnerUi {

	@Test
	public void testSample() {
		// TODO: comment about the returln typo to the original blog
		// https://blog.knoldus.com/ui-testing-with-karate/
		// return Karate.run("sample").relativeTo(getClass());
		Results results = Runner.parallel(getClass(), 1);

		// ("classpath:karate/sample.feature");
	}
}
