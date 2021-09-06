package example;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(MockitoJUnitRunner.class)
public class AppTest {

	@Spy
	// NOTE: not a mock
	private final App sut = new App();

	@Before
	public void setUp() {
		ReflectionTestUtils.setField(sut, "value", "test value");
	}

	@Test
	public void testUpdateUser() throws Exception {
		sut.getValue();
		Mockito.verify(sut, Mockito.times(1)).getValue();
	}

}
