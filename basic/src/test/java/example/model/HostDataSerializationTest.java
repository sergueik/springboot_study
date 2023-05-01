package example.model;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.is;

import java.util.HashMap;
import java.util.Map;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.internal.LinkedTreeMap;

public class HostDataSerializationTest {
	private HostData dataIn = null;
	private HostData dataOut = null;
	private static Gson gson = new GsonBuilder().serializeNulls()
			.disableHtmlEscaping().setLenient().create();
	private String payload = null;
	// private String rawpayload = "{ \"id\":
	// \"172f1d3c-35f2-7aa1-a9a3-d2fb56513b79\", \"securityResourceId\":
	// \"172f1d3c-35ce-8c9b-7ddd-17b769ceb2f1\", \"name\": \"Component 2\",
	// \"description\": \"Component example\", \"created\": 1593195086676,
	// \"componentType\": \"STANDARD\", \"ignoreQualifiers\": 0,
	// \"importAutomatically\": false, \"useVfs\": true, \"active\": true,
	// \"integrationFailed\": false, \"deleted\": false, \"defaultVersionType\":
	// \"FULL\", \"cleanupDaysToKeep\": 0, \"cleanupCountToKeep\": 0, \"tags\":
	// [], \"user\": \"admin\", \"properties\": { \"id\":
	// \"1730c27e-ba73-5ee2-e8ec-25e686478278\", \"name\": \"custom/parameter\",
	// \"value\": \"test\", \"description\": \"name of the property\", \"secure\":
	// false } }";
	private String rawpayload = "{ \"id\": \"172f1d3c-35f2-7aa1-a9a3-d2fb56513b79\", \"securityResourceId\": \"172f1d3c-35ce-8c9b-7ddd-17b769ceb2f1\", \"name\": \"Component  2\", \"description\": \"Component example\", \"created\": 1593195086676, \"componentType\": \"STANDARD\", \"ignoreQualifiers\": 0, \"importAutomatically\": false, \"useVfs\": true, \"active\": true, \"integrationFailed\": false, \"deleted\": false, \"defaultVersionType\": \"FULL\", \"cleanupDaysToKeep\": 0, \"cleanupCountToKeep\": 0, \"tags\": [], \"user\": \"admin\", \"properties\": { \"id\": \"1730c27e-ba73-5ee2-e8ec-25e686478278\", \"name\": \"custom/parameter\", \"value\": \"test\", \"description\": \"name of the property\", \"secure\": false } , \"result\": { \"host\": \"hostname\", \"filePaths\": [\"a\", \"b\"], \"data\": {} }}";

	private Map<String, Object> value = new HashMap<>();

	@Before
	public void setUp() {
		dataIn = new HostData("host1", "c:\\temp", "host1.json");
		System.err.println("Data in: " + dataIn);
	}

	@Test
	public void test2() {
		System.err.println("Raw Payload: " + rawpayload);
		try {
			value = gson.fromJson(rawpayload, Map.class);
			String check = (String) value.get("properties");
			System.err.println("Check: " + check);
		} catch (Exception e) {

			System.err.println("Exception: " + e.toString());
			// Exception: java.lang.ClassCastException:
			// com.google.gson.internal.LinkedTreeMap cannot be cast to
			// java.lang.String
		}

	}

	@Test
	public void test3() {
		try {
			value = gson.fromJson(rawpayload, Map.class);
			LinkedTreeMap<String, Object> check = (LinkedTreeMap<String, Object>) value
					.get("properties");
			System.err.println("Check: " + gson.toJson(check).toString());
		} catch (Exception e) {

			System.err.println("Exception: " + e.toString());
			// Exception: java.lang.ClassCastException:
			// com.google.gson.internal.LinkedTreeMap cannot be cast to
			// java.lang.String
		}

	}

	@Test
	public void test4() {
		try {
			value = gson.fromJson(rawpayload, Map.class);
			LinkedTreeMap<String, Object> check = (LinkedTreeMap<String, Object>) value
					.get("result");
			String payload2 = gson.toJson(check).toString();
			System.err.println("Check: " + payload2);
			dataOut = gson.fromJson(payload2, HostData.class);
		} catch (Exception e) {

			System.err.println("Exception in test4 : " + e.toString());
			// Exception: java.lang.ClassCastException:
			// com.google.gson.internal.LinkedTreeMap cannot be cast to
			// java.lang.String
		}

	}

	// NOTE (!):
	// java.lang.StackOverflowError
	@Ignore
	@Test
	public void test1() {
		try {
			payload = gson.toJson(dataIn);
			System.err.println("Data payload: " + payload.toString());
		} catch (Exception e) {

			System.err.println("Exception: " + e.toString());
		}
		value.clear();
		value.put("result", dataIn);
		value.put("status", true);
		payload = gson.toJson(value);
		System.err.println("Payload: " + payload.toString());
		// Map<String, Object> result = gson.fromJson(payload, Map.class);
		// HostData dataOut = (HostData) result.get("result");
	}

}
