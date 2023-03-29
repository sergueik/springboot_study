package example.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.http.RequestEntity;

import example.service.ExampleService;
import example.utils.ProcessRunner;

public class MockProcessRunnerRunTest {

	// TODO: sort mockito annotations
	// @InjectMocks
	private Controller controller;

	// @InjectMocks
	// @Mock
	private ExampleService mockService;

	// @Mock
	private ProcessRunner mockProcessRunner;

	private final String url = "https://localhost:80/cgi-bin/example.cgi?a%20b%20c";
	private final String body = "{ \"foo\":  \"bar\" }";

	@Before
	public void setup() throws URISyntaxException {
		mockProcessRunner = Mockito.mock(ProcessRunner.class);
		mockService = new ExampleService(mockProcessRunner);
		controller = new Controller(mockService);

		// TODO: The method when(T) in the type Mockito is not applicable for the
		// arguments (void)
		// when(mockProcessRunner.runProcess(any(String.class),
		// eq(body))).thenReturn();
		when(mockProcessRunner.getProcessOutput()).thenReturn("got json");
		when(mockProcessRunner.isStatus()).thenReturn(true);
	}

	// @Ignore
	@Test
	public void test1() throws URISyntaxException {
		RequestEntity<byte[]> request;
		request = RequestEntity.post(new URI(url))
				.body(body.getBytes(StandardCharsets.UTF_8));

		assertThat(controller.status("status1.cgi", (byte[]) request.getBody()),
				is("got json"));
	}
}
