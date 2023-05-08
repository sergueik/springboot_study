package example.model;

import java.util.regex.Pattern;

public class HostName {
	private String hostName = null;
	private String hostPrefix = null;
	private final static Pattern pattern = Pattern.compile("^([^-.]+)[-.]?.*$");

	public HostName(String data) {
		hostName = data;
		generateHostPrefix();
	}

	private void generateHostPrefix() {
		String value = pattern.matcher(hostName).replaceAll("$1");
		hostPrefix = (value.length() < 3) ? value : value.substring(0, 3);
	}

	public String getHostName() {
		return hostName;
	}

	public void setHostName(String data) {
		hostName = data;
	}

	public String getHostPrefix() {
		if (hostPrefix == null)
			generateHostPrefix();

		return hostPrefix;
	}

}
