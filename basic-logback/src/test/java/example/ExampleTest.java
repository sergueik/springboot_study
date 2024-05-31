package example;

import org.junit.Test;
import org.apache.commons.lang3.StringUtils;
import org.assertj.core.util.Arrays;

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
	//
	@Test
	public void contextLoads() {
		int value = Integer.parseInt("10001111001", 2);
		// value = 139;
		System.err.println("Value: " + value);
		String result = Integer.toString(value, 2);
		System.err.println("Value (binary): " + result);
		result = Integer.toBinaryString(value);
		System.err.println("Value (binary): " + result);
		result = intToBinaryString(value);
		System.err.println("Value (binary): " + result);

		System.err.println("Value (check): " + binaryToInteger(result));
		int length = measureGap(value);
		System.err.println("Longest gap: " + length);
		assertTrue(result.contains(StringUtils.repeat("0", length)));
		String mask = Stream.generate(() -> "0").limit(length).collect(Collectors.joining());
		assertTrue(result.contains(mask));
		mask = StringUtils.repeat("0", length);
		assertTrue(result.contains(mask));
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

	// based on:
	// https://stackoverflow.com/questions/10178980/how-to-convert-a-binary-string-to-a-base-10-integer-in-java
	public int binaryToInteger(String binString) {
		String[] digits = binString.split("");
		// System.err.println("digits: " + Arrays.asList(digits));
		int value = 0;
		int pow = 1;
		int count = 0;
		for (int i = digits.length - 1; i >= 0; i--) {
			// System.err.println("Digit: " + digits[i]);
			value += (digits[i].compareTo("1") == 0) ? pow : 0;
			pow = pow << 1;
		}
		return value;

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
	}

	// see also
	// https://stackoverflow.com/questions/5263187/print-an-integer-in-binary-format-in-java
	// https://www.baeldung.com/java-print-integer-binary

	public static String intToBinaryString(int value) {
		StringBuilder result = new StringBuilder();
		if (value == 0) {
			return "0";
		}
		while (true) {
			if (value == 0) {
				result = result.reverse();
				return result.toString();
			}
			int remainder = value % 2;
			// appending
			result.append(remainder);
			value /= 2;
		}
	}

}
