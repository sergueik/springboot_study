package example;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CustomPropertyUpdater implements PropertyUpdater {
	private String configuration = null;
	private Map<String, Object> properties;
	// private static String osName = OSUtils.getOsName();
	private boolean debug = false;
	private boolean trim = false;

	public String getConfiguration() {
		return configuration;
	}

	public void setConfiguration(String value) {
		configuration = value;
	}

	public void setTrim(boolean value) {
		trim = value;
	}

	public void setProperties(Map<String, Object> value) {
		properties = value;
	}

	public void updateConfiguration() {
		configuration = resolveVars(configuration);
	}

	private String resolveVars(String input) {
		if (null == input) {
			return null;
		}
		Pattern p = Pattern.compile(":\\[ *(\\w+) *\\]");
		Matcher m = p.matcher(input);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			String n = m.group(1);
			String v = properties.containsKey(n) ? properties.get(n).toString()
					: System.getenv(n);
			if (trim) {
				m.appendReplacement(sb,
						null == v || v.replaceAll("\"", "").trim().length() == 0 ? ""
								: v.replace("\\", "\\\\"));
			} else {
				m.appendReplacement(sb,
						null == v || v.trim().length() == 0 ? "" : v.replace("\\", "\\\\"));

			}
		}
		m.appendTail(sb);
		return sb.toString();
	}

}
