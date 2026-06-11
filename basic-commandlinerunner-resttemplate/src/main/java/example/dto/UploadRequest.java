package example.dto;

import java.util.Base64;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UploadRequest {
	private static final Logger logger = LoggerFactory.getLogger(UploadRequest.class);

	private int id;

	public int getId() {
		return id;
	}

	public void setId(int value) {
		this.id = value;
	}

	private String foo;
	private String bar;

	private String filename;
	private String contentType;
	private String contentBase64;
	private String data = null;

	public String getFoo() {
		return foo;
	}

	public void setFoo(String value) {
		foo = value;
	}

	public String getBar() {
		return bar;
	}

	public void setBar(String value) {
		bar = value;
	}

	public String getFilename() {
		return filename;
	}

	public void setFilename(String value) {
		filename = value;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String value) {
		contentType = value;
	}

	public String getContentBase64() {
		return contentBase64;
	}

	public void setContentBase64(String value) {
		contentBase64 = value;
		try {
			byte[] byteContent = Base64.getDecoder().decode(contentBase64);
			data = new String(byteContent);
		} catch (IllegalArgumentException e) {
			logger.error("Could not load base 64 content");
			throw e;
		}
	}

	public String toString() {
		return String.format("foo = %s bar = %s filename = %s data: %d bytes", this.getFoo(), this.getBar(),
				this.getFilename(), this.data.length());
	}

	private static String staticInfo;

	public /* static */ String getStaticInfo() {
		return UploadRequest.staticInfo;
	}

}
