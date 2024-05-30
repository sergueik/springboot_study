package example;

import org.junit.Test;
import org.apache.commons.lang3.StringUtils;

import static org.junit.Assert.assertTrue;

import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.Ignore;

import org.junit.runner.RunWith;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

// Spring Framwork Release sensitive area
// @RunWith(SpringJUnit4ClassRunner.class)
// @ExtendWidth(SpringRunner.class) 
// for Junit 5
// @SpringApplicationConfiguration(classes = Example.class)
// @WebAppConfiguration
@RunWith(SpringRunner.class)
@SpringBootTest
public class ExampleTest {
	@Test
	public void contextLoads() {
		int value = 1145;
		String result = Integer.toString(value, 2);
		System.err.println("Result: " + result);
		// value = value >> 1;
		// result = Integer.toBinaryString(value);
		// System.err.println("Result: " + result);
		int length = measureGap(value);
		assertTrue(result.contains(StringUtils.repeat("0", length)));

		Stream.generate(() -> "0").limit(length).collect(Collectors.joining());
		// assertTrue(result.contains("0".repeat(length)));
	}

	public int measureGap1(int value) {

		int best = 0;
		int gap = 0;
		System.err.println("input: " + Integer.toString(value, 2));
		// shift left as long as minor digit is zero
		while (value != 0 && (value & 1) == 0) {
			value = value >> 1;
		}

		while (value != 0) {
			if ((value & 1) == 0) {
				gap++;
				best = Math.max(gap, best);
			} else {
				// reset gap
				gap = 0;
				System.err.println("Reset gap. Best: " + best);
			}
			value = value >> 1;
		}
		System.err.println("Best: " + best);
		return best;
	}

	public int measureGap(int value) {
		int best = 0;
		int gap = 0;

		System.err.println("input: " + Integer.toString(value, 2));
		// shift left as long as minor digit is zero
		while (value != 0 && (value & 1) == 0) {
			value = value >> 1;
		}

		while (true) {
			if ((value & 1) == 0) {
				gap++;
			} else {
				best = Math.max(gap, best);
				// reset gap
				gap = 0;
				System.err.println("Reset gap. Best: " + best);
			}
			value = value >> 1;
			if (value == 0) {
				System.err.println("Best: " + best);
				return best;
			}
		}
		// return 0;
	}

	// see also
	// https://stackoverflow.com/questions/5263187/print-an-integer-in-binary-format-in-java
	public static String intToString(int number, int groupSize) {
		StringBuilder result = new StringBuilder();

		for (int i = 31; i >= 0; i--) {
			int mask = 1 << i;
			result.append((number & mask) != 0 ? "1" : "0");

			if (i % groupSize == 0)
				result.append(" ");
		}
		result.replace(result.length() - 1, result.length(), "");

		return result.toString();
	}

}
