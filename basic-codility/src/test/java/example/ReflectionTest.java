package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.startsWith;

import java.lang.reflect.Field;
import org.junit.Assume;
import org.junit.Test;

// https://stackoverflow.com/questions/20945049/is-a-java-string-really-immutable
public class ReflectionTest {

	private boolean debug = false;

	private String data = "Hello World";

	// https://www.baeldung.com/java-immutable-object
	@Test
	public void test1() {
		String newData = data.replace("World", "Java!");

		assertThat(data, is("Hello World"));
		assertThat(newData, is("Hello Java!"));
	}

	// NOTE: works on Java 8, but fails on Java 11:
	// Tests in error:
	//  test2(example.ReflectionTest): class [B cannot be cast to class [C ([B and [C are in module java.base of loader 'bootstrap')

	@Test
	public void test2()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
	        String javaVersion = System.getProperty("java.version");
	        Assume.assumeThat(javaVersion, startsWith("1.8"));
		Field field = String.class.getDeclaredField("value");
		field.setAccessible(true);
		char[] value = (char[]) field.get(data);
		value[6] = 'J';
		value[7] = 'a';
		value[8] = 'v';
		value[9] = 'a';
		value[10] = '!';
		System.err.println("test2: " + data);
		assertThat(data, is("Hello Java!"));
	}

}
