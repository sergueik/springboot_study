package example;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Utils {

	private static Utils utils;

	public static Utils getInstance() {
		if (utils == null) {
			utils = new Utils();
		}
		return utils;
	}

	private Utils() {
	}

	public static List<Integer> range(int start, int end) {
		return IntStream.range(start, end).boxed().collect(Collectors.toList());
	}

	public static String name() {
		return "utils";
	}

	public static Helper getHelper() {
		return new Helper();

	}

	public String operation() {
		return "operation";
	}

	public Helper getInstanceHelper() {
		return new Helper();
	}
}
