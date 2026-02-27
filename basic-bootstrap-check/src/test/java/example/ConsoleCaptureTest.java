package example;

import org.junit.Before;
import org.junit.Ignore;
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

	// based on:
	// see also:
	// https://www.tabnine.com/code/java/classes/org.springframework.boot.test.rule.OutputCapture

	private final static ExampleService sut = new ExampleService();
	// JUnit 4 @Rule to capture output from System.out and System.err.
	// see also:
	// https://docs.spring.io/spring-boot/docs/current/api/org/springframework/boot/test/system/OutputCaptureRule.html
	@Rule
	public OutputCapture capture = new OutputCapture();

	@Test
	public void test1() throws Exception {
		Assume.assumeTrue(true);
		sut.hello();
		assertThat(capture.toString(), containsString("Hello basic"));
	}

	@Ignore("trouble with multiline")
	@Test
	public void test2() throws Exception {
		Assume.assumeTrue(true);
		sut.hello();
		assertThat(capture.toString(), containsString("Hello basic\n"));
	}
}
