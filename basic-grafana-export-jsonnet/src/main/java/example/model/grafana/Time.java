package example.model.grafana;

import com.google.gson.annotations.SerializedName;

// generated via https://json2csharp.com/code-converters/json-to-pojo
public class Time {
	@SerializedName("from")
	public String from;
	@SerializedName("to")
	public String myto;

	public String getFrom() {
		return from;
	}

	public void setFrom(String data) {
		from = data;
	}

	public String getMyto() {
		return myto;
	}

	public void setMyto(String data) {
		myto = data;
	}
}
