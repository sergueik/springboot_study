package example;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.codec.binary.Base64;

import java.io.UnsupportedEncodingException;

import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonObject.Member;
import com.eclipsesource.json.JsonValue;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonValue;
import com.eclipsesource.json.JsonObject.Member;

@Component
public class Params {

	private static final boolean debug = true;
	private static final String paramKeysValidator = "(?:id|name|success|result)";
	
	// system property
	@Value("${params:eyJpZCI6MH0K}") // {"id": 0}
	private String value;

	// hidden
	private String appname;
	private int id = -1;
	private int result = -1;

	public int getId() {
		if (id == -1) {
			Map<String, JsonValue> params = parseValue();
			id = params.get("id").asInt();
		}
		return id;
	}

	public int getResult() {
		if (result == -1) {
			Map<String, JsonValue> params = parseValue();
			result = params.get("result").asInt();
		}
		return result;
	}

	// TODO: Params.toString()

	// parse the value
	private Map<String, JsonValue> parseValue() {

		Map<String, JsonValue> params = new HashMap<>();
		readSideData(decodePropertyArgument(this.value), Optional.of(params),
				paramKeysValidator);
		return params;
	}

	public String getAppname() {
		if (appname == null) {
			Map<String, JsonValue> params = parseValue();
			appname = (String) params.get("name").asString();
		}
		return appname;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String data) {
		this.value = data;
	}

	// based on:
	// https://www.programcreek.com/java-api-examples/org.apache.commons.codec.binary.Base64
	private String decodePropertyArgument(String rawData) {
		String decodedData = null;
		if (rawData != null) {
			try {
				decodedData = new String(Base64.decodeBase64(rawData.getBytes("UTF8")));
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
			}
		}
		return decodedData;
	}

	private String readSideData(String payload,
			Optional<Map<String, JsonValue>> parameters, String acceptedKeys) {
		if (debug) {
			System.err.println("Accepted keys: " + acceptedKeys);
		}
		Map<String, JsonValue> collector = (parameters.isPresent())
				? parameters.get() : new HashMap<>();

		String data = (payload == null)
				? "{\"foo\":\"bar\", \"result\":true,\"id\":42 }" : payload;

		if (debug) {
			// System.err.println("Processing payload: " + data.replaceAll(",",
			// ",\n"));
			System.err.println("Processing payload: [ " + data + "]");
		}

		JsonObject jsonObject = JsonObject.readFrom(data);
		Iterator<Member> jsonObjectIterator = jsonObject.iterator();

		while (jsonObjectIterator.hasNext()) {
			Member jsonObjectMember = jsonObjectIterator.next();
			System.err.println("Found member: " + jsonObjectMember.getName());
			String propertyKey = jsonObjectMember.getName();
			if (!propertyKey.matches(acceptedKeys)) {
				System.err.println("Ignoring key: " + propertyKey);
				continue;
			}
			if (debug) {
				System.err.println("Processing key: " + propertyKey);
			}
			Boolean found = false;
			try {
				JsonValue propertyVal = jsonObject.get(propertyKey);
				if (debug) {
					System.err
							.println("Loaded string: " + propertyKey + ": " + propertyVal);
				}
				collector.put(propertyKey, propertyVal);
				found = true;
			} catch (Exception e) {
				System.err.println("Exception (ignored, continue): " + e.toString());
			}
		}
		return Integer.toString(collector.get("id").asInt());
	}
}
