package example;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.mockito.MockedStatic;
import org.mockito.Mockito;
import static org.mockito.Mockito.when;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.greaterThan;

public class AppMethod2Test {

	// @Spy
	private App sut;
	MockedStatic<Utils> utils;

	private final static String value = "test";
	private Helper helper = Mockito.mock(Helper.class);

	@BeforeEach
	public void setUp() {
	}

	@Disabled
	@Test
	public void test1() throws Exception {
		try (MockedStatic<Utils> utils = Mockito.mockStatic(Utils.class)) {
			utils.when(Utils::getHelper).thenReturn(helper);
			sut = new App();
			when(helper.operation()).thenReturn("mock: " + value);
			assertThat(sut.getHelperOperation(), containsString(value));
			// To create a new mock, the existing static mock registration must be
			// deregistered
			utils.close();
		}
	}

}
