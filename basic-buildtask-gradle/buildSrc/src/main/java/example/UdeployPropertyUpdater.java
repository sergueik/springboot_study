package example;

/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class UdeployPropertyUpdater implements PropertyUpdater {
	private String configuration = null;
	private Map<String, Object> properties;
	private boolean trim = false;

	public String getConfiguration() {
		return configuration;
	}

	public void setConfiguration(String value) {
		configuration = value;
	}

	public void setProperties(Map<String, Object> value) {
		properties = value;
	}

	public void setTrim(boolean value) {
		trim = value;
	}

	public void updateConfiguration() {
		properties.keySet().stream().forEach((String name) -> {
			Object value = properties.get(name);
			List<String> results = Arrays.asList(configuration.split("\r?\n"))
					.stream().map((String line) -> replaceEntry(line, name, value))
					.collect(Collectors.toList());
			configuration = String.join("\n", results);
		});

		System.err.println("new configuration: " + configuration);
	}

	private String replaceEntry(String payload, String name, Object value) {
		final String expression1 = "^.*\\{\\{\\*" + "(.+)" + "\\*\\}\\}.*$";
		Pattern p = Pattern.compile(expression1);
		// System.err.println(String.format("Pattern exression %s:\n",
		// p.toString()));
		String input = payload;
		Matcher m = p.matcher(input);

		if (m.find()) {

			// System.err.println("group count: " + m.groupCount());
			// java.lang.IllegalArgumentException: No group with name <value>
			String captured1 = m.group(1).toString().trim();
			// System.err.println(String.format("group (1): \"%s\"", captured1));
			final String expression2 = String.format("^%s\\|\\|\\|([a-zA-Z0-9.:/\\_-]+)$", name);
			p = Pattern.compile(expression2);
			// System.err
			// .println(String.format("Pattern exression %s:\n", p.toString()));
			input = captured1;
			m = p.matcher(input);
			if (m.find()) {
				final String expression3 = String
						.format("\\{\\{\\*(?: )*" + "%s\\|\\|\\|([a-zA-Z0-9.:/\\_-]+)" + "(?: )*\\*\\}\\}.*$", name);
				if (value != null
						&& !value.toString().replaceAll("[\"']", "").isEmpty()) {
					// System.err.println(String.format("Replacement \"%s\"",
					// value.replaceAll("[\"']", "")));
					return payload.replaceAll(expression3, value.toString());
				} else {
					// System.err.println("groups: " + m.groupCount());
					String captured2 = m.group(1).toString();
					return payload.replaceAll(expression3, captured2);
				}
			} else {
				// System.err.println("nothing found in " + input);
				return payload;
			}

		} else {
			// System.err.println("nothing found in " + input);
			return payload;
		}
	}

}
