package ge.vakho.native_messaging.protocol;

import jakarta.xml.bind.annotation.XmlElement;
// import javax.xml.bind.annotation.XmlElement;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class NativeRequest {

	@XmlElement(name = "message")
	private String message;

	public NativeRequest() {
		super();
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}
}
