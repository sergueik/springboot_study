package example;

import java.util.UUID;

public class Header {

	private String traceID;
	private static String staticInfo;

	public String getTraceID() {
		return traceID;
	}

	public void setTraceID(String data) {
		if (data != null) {
			System.err.println("setting traceid: " + data);
			traceID = data;
		}
	}

	public Header() {
		staticInfo = UUID.randomUUID().toString();
	}

	public String getStaticInfo() {
		return Header.staticInfo;
	}

	public Header(String traceID) {
		super();
		if (Header.staticInfo == null) {
			Header.staticInfo = UUID.randomUUID().toString();
		}
		if (traceID != null) {
			this.traceID = traceID;
		}
	}
}
