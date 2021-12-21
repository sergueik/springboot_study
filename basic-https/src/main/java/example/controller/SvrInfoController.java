package example.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import org.json.JSONObject;
import org.json.JSONArray;
import org.json.JSONException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.json.CDL;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

// TODO: mvn case
// @Controller
@RestController
@RequestMapping(path = "/")
public class SvrInfoController {
	private final Logger logger = LoggerFactory
			.getLogger(SvrInfoController.class);

	@ResponseBody
	@GetMapping(path = "/svrinfo")
	public String svrinfo() {
		logger.info("\"/svrinfo\" called");
		String result = null;
		// Apparently CDL cannot convert this:
		String dataRowsJSON = "{\"rows\": [[\"one\", \"E\", true, true],[\"two\", \"E\", true, true]]}";

		dataRowsJSON = "[[\"one\", \"E\", true, true],[\"two\", \"E\", true, true]]";

		dataRowsJSON = "[{\"name\":\"one\", \"datacenter\":\"E\", \"status\": true, \"primary\":true},{\"name\":\"two\", \"datacenter\":\"E\", \"status\": true, \"primary\":true}]";
		final Gson gson = new GsonBuilder().setPrettyPrinting().create();
		Data[] data = gson.fromJson(dataRowsJSON, Data[].class);
		logger.info(String.format("Deserialized data %d rows", data.length));

		dataRowsJSON = "{\"rows\": [{\"name\":\"one\", \"datacenter\":\"E\", \"status\": true, \"primary\":true},{\"name\":\"two\", \"datacenter\":\"E\", \"status\": true, \"primary\":true}]}";
		// can convert this via CDL, but unknown how to make headers disappear:
		try {
			result = dumpJSONCsv(new JSONObject(dataRowsJSON).getJSONArray("rows"));
		} catch (JSONException e) {
			logger.info("Exception bulding response: " + e.toString());
		}
		List<Data> dataRows = Arrays.asList(new Data[] {
				new Data("one", "E", true, true), new Data("two", "E", true, true),
				new Data("three", "W", true, true),
				new Data("four", "W", true, true) });
		result = dumpCsv(dataRows);
		return result;
	}

	private String dumpCsv(List<Data> dataRows) {
		String result = null;
		result = String.join("\n",
				dataRows.stream()
						.map(o -> String.join(",",
								Arrays.asList(o.getName(), o.getDatacenter(),
										String.format("%b", o.getPrimary()),
										String.format("%b", o.getStatus()))))
						.collect(Collectors.toList()));
		logger.info("Result: " + result);
		return result;
	}

	// NOTE: jackson is deprecated
	//
	// https://www.baeldung.com/java-converting-json-to-csv
	// see also:
	// https://www.tutorialspoint.com/how-to-convert-a-json-array-to-csv-in-java
	// https://stleary.github.io/JSON-java/org/json/CDL.html
	private String dumpJSONCsv(final JSONArray dataRows) {
		String result = null;
		try {
			logger.info("converting data: " + dataRows);
			result = CDL.toString(dataRows);
			logger.info("Result: " + result);
		} catch (JSONException e) {
			logger.info("Exception bulding result: " + e.toString());
		}
		return result;
	}

	public static class Data {

		private String name;

		public String getName() {
			return name;
		}

		public void setName(String value) {
			name = value;
		}

		private String datacenter;

		public String getDatacenter() {
			return datacenter;
		}

		public void setDatacenter(String value) {
			datacenter = value;
		}

		private boolean primary;

		public boolean getPrimary() {
			return primary;
		}

		public void setPrimary(boolean value) {
			primary = value;
		}

		private boolean status;

		public boolean getStatus() {
			return status;
		}

		public void setStatus(boolean value) {
			status = value;
		}

		public Data(String name, String datacenter, boolean primary,
				boolean status) {
			this.primary = primary;
			this.status = status;
			this.name = name;
			this.datacenter = datacenter;
		}

		public Data() {
		}
	}
}
