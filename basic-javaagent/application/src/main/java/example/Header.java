package example;

import java.util.UUID;

public class Header {

	private String name;
	private String val;
	private static String staticInfo;

	public String getName() {
		return name;
	}

	public void setName(String data) {
		name = data;
	}

	public String getVal() {
		return val;
	}

	public void setVal(String data) {
		val = data;
	}

	public Header() {
		staticInfo = UUID.randomUUID().toString();
	}

	public String getStaticInfo() {
		return Header.staticInfo;
	}

	public Header(String name, String val) {
		super();
		if (Header.staticInfo == null) {
			Header.staticInfo = UUID.randomUUID().toString();
		}
		this.name = name;
		this.val = val;
	}
}
