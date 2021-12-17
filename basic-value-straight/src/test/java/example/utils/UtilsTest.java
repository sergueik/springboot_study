package example.utils;

import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Before;
import org.junit.Test;

public class UtilsTest {
	private Utils utils;

	@Before
	public void before() {

		utils = new Utils();
	}

	@Test
	public void test1() {
		assertThat(utils.getValueConfig(), nullValue());
	}

	@Test
	public void test2() {
		assertThat(utils.getValue(), is("Value for test"));
	}
}
