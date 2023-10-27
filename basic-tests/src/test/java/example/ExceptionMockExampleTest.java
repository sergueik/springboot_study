package example;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;

// based on: https://www.baeldung.com/mockito-exceptions#non-void-return-type
@SpringBootTest
public class ExceptionMockExampleTest {

	private Data data = Mockito.mock(Data.class);

	private static class Data {

		private Map<String, String> map;

		public void add(String word, String meaning) {
			map.put(word, meaning);
		}

		public String get(String word) {
			return map.get(word);
		}
	}

	@BeforeEach
	public void setup() {

		when(data.get(any(String.class))).thenReturn("");
		when(data.get("word")).thenThrow(NullPointerException.class);
		Mockito.doThrow(new IllegalStateException("error")).when(data)
				.add(any(String.class), any(String.class));
		// NOTE: "unfinished stubbing" compiler error in below
		// but Eclipse does not always indicate syntax error in misplaced
		// parenthesis
		// Mockito.doThrow(new IllegalStateException("error"))
		// .when(data.add(any(String.class), any(String.class)));

	}

	@Test
	public void test1() {
		String result = data.get("something");
		assertThat(result, notNullValue());
	}

	@Test
	public void test2() {
		assertThrows(NullPointerException.class, () -> data.get("word"));
	}

	@Test
	public void test3() {

		Exception exception = assertThrows(Exception.class, () -> {
			data.add("key", "value");
		});
		assertThat(exception.getMessage(), containsString("error"));
	}

}
