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

import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.MockedStatic;

import static org.mockito.Mockito.when;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.greaterThan;

public class AppMethod3Test {

	// @Spy
	private App sut;
	MockedStatic<Utils> utils;

	private final static String value = "test";
	private Utils mockUtils = Mockito.mock(Utils.class);

	@BeforeEach
	public void setUp() {
		when(mockUtils.operation()).thenReturn("mock: " + value);
	}

	@Disabled
	@Test
	public void test1() throws Exception {
		utils = Mockito.mockStatic(Utils.class);
		utils.when(Utils::getInstance).thenReturn(mockUtils);
		sut = new App();
		assertThat(sut.getUtilsOperation(), containsString(value));
		// To create a new mock, the existing static mock registration must be
		// deregistered
		utils.close();
	}

}
