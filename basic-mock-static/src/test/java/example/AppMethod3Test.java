package example;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

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

// https://qna.habr.com/q/1142084
@RunWith(MockitoJUnitRunner.class)
public class AppMethod3Test {

	// @Spy
	private App sut;
	MockedStatic<Utils> utils;

	private final static String value = "test";
	private Utils mockUtils = Mockito.mock(Utils.class);

	@Before
	public void setUp() {
		utils = Mockito.mockStatic(Utils.class);
		utils.when(Utils::getInstance).thenReturn(mockUtils);
		when(mockUtils.operation()).thenReturn("mock: " + value);

		sut = new App();
	}

	@After
	public void close() {
		utils.close();
	}

	@Test
	public void test1() throws Exception {
		assertThat(sut.getUtilsOperation(), containsString(value));
	}

}
