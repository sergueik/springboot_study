package example;

import org.junit.Before;
import org.junit.Test;
import org.junit.Rule;
import org.junit.Assume;
import org.springframework.boot.test.rule.OutputCapture;

// NOTE: fragile and may conflict with other tests
//captures STDERRR too

// see also:
// https://www.codota.com/code/java/classes/org.springframework.boot.test.rule.OutputCapture

import example.service.ExampleService;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;

public class ConsoleCaptureTest {

	private final static ExampleService sut = new ExampleService();
	@Rule
	public OutputCapture capture = new OutputCapture();

	@Test
	public void testName() throws Exception {
		Assume.assumeTrue(true);
		sut.hello();
		assertThat(capture.toString(), containsString("basic"));
	}
}
