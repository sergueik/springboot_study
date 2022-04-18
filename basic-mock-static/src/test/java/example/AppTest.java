package example;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.mockito.Mockito;
import org.mockito.MockedStatic;

import static org.mockito.Mockito.when;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.greaterThan;

// NOTE: no need in @ExtendWith(...) for Junit 5.x
public class AppTest {

	// @Spy
	private App sut;

	private final static String value = "test";
	private Helper helper = Mockito.mock(Helper.class);

	@BeforeAll
	public void setUp() {
		when(helper.operation()).thenReturn("mock: " + value);
	}
	
	@Disabled
	@Test
	public void test1() throws Exception {
		MockedStatic<Utils> utils = Mockito.mockStatic(Utils.class);
		utils.when(Utils::name).thenReturn(value);
		sut = new App();
		assertThat(sut.getValue(), is(value));
		// To create a new mock, the existing static mock registration must be
		// deregistered
		utils.close();

	}

	@Disabled
	@Test
	public void test2() throws Exception {
		MockedStatic<Utils> utils = Mockito.mockStatic(Utils.class);
		utils.when(Utils::getHelper).thenReturn(helper);
		sut = new App();
		assertThat(sut.getHelperOperation(), containsString(value));
		// To create a new mock, the existing static mock registration must be
		// deregistered
		utils.close();
	}

}
