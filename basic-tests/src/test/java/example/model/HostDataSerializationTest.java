package example.model;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.is;

import java.util.HashMap;
import java.util.Map;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

public class HostDataSerializationTest {
	private HostData dataIn = null;
	private HostData dataOut = null;
	private static Gson gson = new GsonBuilder().serializeNulls()
			.disableHtmlEscaping().setLenient().create();
	private String payload = null;
	Map<String, Object> value = new HashMap<>();

	// StackOverflow
	@BeforeEach
	public void setUp() {
		/*
		dataIn = new HostData("host1", "c:\\temp", "host1.json");
		System.err.println("Data in: " + dataIn);
		try {
			// NOTE: cannot
			// java.lang.StackOverflowError:
			payload = gson.toJson(dataIn);
			System.err.println("Data payload: " + payload);
		} catch (Exception e) {
		
			System.err.println("Exception: " + e.toString());
		}
		value.clear();
		value.put("result", dataIn);
		value.put("status", true);
		payload = gson.toJson(value);
		System.err.println("Payload: " + payload);
		*/
	}

	@Test
	public void test1() {
		// Map<String, Object> result = gson.fromJson(payload, Map.class);
		// HostData dataOut = (HostData) result.get("result");
	}

}
