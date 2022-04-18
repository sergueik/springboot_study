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
public class AppTest {

	// @Spy
	private App sut;
	MockedStatic<Utils> utils;

	private final static String value = "test";
	private Helper helper = Mockito.mock(Helper.class);

	@Before
	public void setUp() {
		when(helper.operation()).thenReturn("mock: " + value);
	}

	@Test
	public void test1() throws Exception {
		utils = Mockito.mockStatic(Utils.class);
		utils.when(Utils::name).thenReturn(value);
		sut = new App();
		assertThat(sut.getValue(), is(value));
		utils.close();

	}

	@Test
	public void test2() throws Exception {
		utils = Mockito.mockStatic(Utils.class);
		utils.when(Utils::getHelper).thenReturn(helper);
		sut = new App();
		assertThat(sut.getHelperOperation(), containsString(value));
		utils.close();

	}

}
