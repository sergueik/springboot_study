package example;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class AppMethod1BrokenTest {

	// @Spy
	private App sut;
	MockedStatic<Utils> utils;

	private final static String value = "test";

	@BeforeEach
	public void setUp() {
		utils = Mockito.mockStatic(Utils.class);
	}

	@AfterEach
	// To create a new mock, the existing static mock registration must be
	// deregistered
	public void closeMock() {
		utils.close();
	}

	@Disabled
	@Test
	public void test1() throws Exception {
		utils.when(Utils::name).thenReturn(value);
		sut = new App();
		assertThat(sut.getValue(), is(value));
	}
}
