
package example;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import java.util.List;
import java.util.Map;

// Proxy mapping class
class FieldMapper {
	private final Map<String, String> copybookToRegex = new HashMap<>();
	private final Map<String, String> regexToCopybook = new HashMap<>();

	public void add(String copybookName) {
		// Generate regex-friendly name: remove underscores, keep letters/digits only
		String regexName = copybookName.replaceAll("_", "");
		copybookToRegex.put(copybookName, regexName);
		regexToCopybook.put(regexName, copybookName);
	}

	public String toRegex(String copybookName) {
		return copybookToRegex.get(copybookName);
	}

	public String toCopybook(String regexName) {
		return regexToCopybook.get(regexName);
	}
}
