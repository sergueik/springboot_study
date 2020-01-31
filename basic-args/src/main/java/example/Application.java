package example;

import static java.lang.System.err;

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
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Component
@RestController
@RequestMapping("/basic")
public class Application {
	private static final boolean debug = false;

	@Value("${params}")
	private String params;

	// hidden
	private String appname;

	private static final Logger logger = LoggerFactory
			.getLogger(CommandLineConfiguration.class);

	public void logConfiguration() {
		logger.info("Loaded with params: " + params);
	}

	// based on:
	// https://www.programcreek.com/java-api-examples/org.apache.commons.codec.binary.Base64
	public String decodePropertyArgument(String rawData) {
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

	public String readSideData(String payload,
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

	@GetMapping
	public String Hello() {
		readSideData(decodePropertyArgument(params),
				Optional.<Map<String, Object>> empty(), "(?:id|name|success|result)");
		Map<String, Object> result = new HashMap<String, Object>();
		readSideData(decodePropertyArgument(params), Optional.of(result),
				"(?:id|name|success|result)");

		return "This is " + result.get("name");
	}
}
