package example;

import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.codec.binary.Base64;
import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.codec.binary.Base64;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Component
public class Params {

	private static final boolean debug = true;
	private static final String paramKeysValidator = "(?:id|name|success|result)";

	@Value("${params}")
	private String value;

	// hidden
	private String appname;
	private int id = -1;
	private int result = -1;

	public int getId() {
		if (id == -1) {
			Map<String, Object> params = parseValue();
			id = Integer.parseInt((String) params.get("id"));
		}
		return id;
	}

	public int getResult() {
		if (result == -1) {
			Map<String, Object> params = parseValue();
			result = Integer.parseInt((String) params.get("result"));
		}
		return result;
	}

	// TODO: Params.toString()
	
	// parse the value
	private Map<String, Object> parseValue() {

		readSideData(decodePropertyArgument(this.value),
				Optional.<Map<String, Object>> empty(), paramKeysValidator);
		Map<String, Object> params = new HashMap<String, Object>();
		readSideData(decodePropertyArgument(this.value), Optional.of(params),
				paramKeysValidator);
		return params;
	}

	public String getAppname() {
		if (appname == null) {
			Map<String, Object> params = parseValue();
			appname = (String) params.get("name");
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
			Optional<Map<String, Object>> parameters, String acceptedKeys) {
		if (debug) {
			System.err.println("Accepted keys: " + acceptedKeys);
		}

		Map<String, Object> collector = (parameters.isPresent()) ? parameters.get()
				: new HashMap<>();

		String data = (payload == null)
				? "{\"foo\":\"bar\", \"result\":true,\"id\":42 }" : payload;
		if (debug) {
			System.err.println("Processing payload: " + data.replaceAll(",", ",\n"));
		}
		try {
			JSONObject elementObj = new JSONObject(data);
			@SuppressWarnings("unchecked")
			Iterator<String> propIterator = elementObj.keys();
			while (propIterator.hasNext()) {

				String propertyKey = propIterator.next();
				if (!propertyKey.matches(acceptedKeys /* "(?:id|name|url|tests)" */)) {
					System.err.println("Ignoring key: " + propertyKey);
					continue;
				}
				if (debug) {
					System.err.println("Processing key: " + propertyKey);
				}
				Boolean found = false;
				try {
					String propertyVal = (String) elementObj.getString(propertyKey);
					// logger.info(propertyKey + ": " + propertyVal);
					if (debug) {
						System.err
								.println("Loaded string: " + propertyKey + ": " + propertyVal);
					}
					collector.put(propertyKey, propertyVal);
					found = true;
				} catch (JSONException e) {
					System.err.println("Exception (ignored, continue): " + e.toString());
				}
				if (found) {
					continue;
				}
				try {
					org.json.JSONArray propertyArrayVal = elementObj
							.getJSONArray(propertyKey);
					int length = propertyArrayVal.length();
					if (debug) {
						System.err.println("Can process array of size: " + length);
					}
					StringBuffer innerData = new StringBuffer();
					for (int index = 0; index < length; index++) {
						JSONObject rowObject = propertyArrayVal.getJSONObject(index);
						if (debug) {
							System.err.println("Can process object: " + rowObject.toString());
						}
						// "comment,id,value,command,target"
						readSideData(rowObject.toString(),
								Optional.<Map<String, Object>> empty(),
								"(?:comment|id|value|command|target)");

						Iterator<String> rowObjectIterator = rowObject.keys();

						while (rowObjectIterator.hasNext()) {
							String rowObjectKey = rowObjectIterator.next();
							innerData.append(String.format("%s,", rowObjectKey));
							if (debug) {
								System.err.println("Processing Row key: " + rowObjectKey);
							}
						}
					}
					collector.put(propertyKey, innerData.toString());
					found = true;
				} catch (JSONException e) {
					System.err.println("Exception (ignored, continue): " + e.toString());
				}
			}
		} catch (JSONException e) {
			System.err.println("Exception (ignored, aborting): " + e.toString());
			return null;
		}
		return (String) collector.get("id");
	}

}
