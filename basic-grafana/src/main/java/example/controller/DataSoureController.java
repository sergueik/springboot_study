package example.controller;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import example.component.AnnotationRequest;
import example.component.AnnotationResponseRow;
import example.component.SearchRequest;
import example.service.ExampleService;

@RestController
@RequestMapping("/")
public class DataSoureController {

	private final static Long high = 42L;
	private final static Long low = 26L;

	// @Autowired
	private ExampleService service;

	// for mocking
	public DataSoureController(ExampleService data) {
		service = data;
	}

	private static final Logger logger = LogManager
			.getLogger(DataSoureController.class);

	@RequestMapping(value = "/", method = RequestMethod.GET, produces = MediaType.TEXT_PLAIN_VALUE)
	@ResponseBody
	public ResponseEntity<Object> heathcheck() {
		logger.info("processing GET /");
		HttpHeaders headers = addResponseHeaders();
		return ResponseEntity.status(HttpStatus.OK).headers(headers)
				.contentType(MediaType.TEXT_PLAIN).body(null);
		/*
		return ResponseEntity.status(HttpStatus.OK).headers(headers)
				.header("Access-Control-Allow-Headers", "accept, content-type")
				.header("Access-Control-Allow-Methods", "POST")
				.header("Access-Control-Allow-Origin", "*")
				.contentType(MediaType.TEXT_PLAIN).body(null);
				*/
	}

	@RequestMapping(method = RequestMethod.POST, value = "/search", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public List<String> postSearchRequest(@RequestBody SearchRequest data) {
		List<String> result = new ArrayList<>();
		result.add("test");
		return result;
	}

	@RequestMapping(value = "/search", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public List<String> Search(HttpServletResponse response) {
		logger.info("processing POST /search");
		addResponseHeaders(response);
		/*
		response.setHeader("Access-Control-Allow-Headers", "accept, content-type");
		response.setHeader("Access-Control-Allow-Methods", "POST");
		response.setHeader("Access-Control-Allow-Origin", "*");
		*/
		List<String> result = new ArrayList<String>();
		result.add("data series");
		return result;
	}

	@SuppressWarnings("unchecked")
	@RequestMapping(value = "/query", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public ResponseEntity<List<Map<String, Object>>> Query(
			@RequestBody Map<String, Object> params) throws JSONException {

		logger
				.info("processing POST /query\n" + new JSONObject(params).toString(4));
		List<Map<String, String>> targets = (List<Map<String, String>>) params
				.get("targets");

		Map<String, String> range = (Map<String, String>) params.get("range");
		String from = range.get("from");
		String to = range.get("to");
		Long fromEpochMillisec = 1000 * Instant.parse(from).getEpochSecond();
		Long toEpochMillisecond = 1000 * Instant.parse(to).getEpochSecond();
		List<Map<String, Object>> result = new ArrayList<>();
		for (Map<String, String> targetMap : targets) {
			String target = (String) targetMap.get("target");
			Map<String, Object> resultRow = new HashMap<>();
			List<Long> datapoint1 = new ArrayList<Long>();
			datapoint1.add(high);
			datapoint1.add(fromEpochMillisec);
			List<Long> datapoint2 = new ArrayList<Long>();
			datapoint2.add(low);
			datapoint2.add(toEpochMillisecond);

			List<List<Long>> datapoints = new ArrayList<>();
			datapoints.add(datapoint1);
			datapoints.add(datapoint2);
			resultRow.put("datapoints", datapoints);

			resultRow.put("target", target);
			switch (target) {
			case "data series":
				result.add(resultRow);
				break;
			default:
			}
		}
		final HttpHeaders headers = new HttpHeaders();
		Collections.sort(result, (o1, o2) -> {
			String name1 = String.valueOf(o1.get("target").toString());
			String name2 = String.valueOf(o2.get("target").toString());
			return name1.compareTo(name2);
		});

		return ResponseEntity.status(HttpStatus.OK).headers(headers)
				.contentType(MediaType.TEXT_PLAIN).body(result);

	}

	@RequestMapping(method = RequestMethod.POST, value = "/annotations", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public List<AnnotationResponseRow> postAnnotationRequest(
			@RequestBody AnnotationRequest data) {
		List<AnnotationResponseRow> response = new ArrayList<>();
		AnnotationResponseRow row = new AnnotationResponseRow();
		row.setAnnotation(data.getAnnotation());
		response.add(row);
		// adding an empty row
		response.add(new AnnotationResponseRow());
		return response;

	}

	@RequestMapping(value = "/annotations", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public ResponseEntity<Map<String, String>> annotations() {
		logger.info("processing POST /annotations");
		Map<String, String> data = new HashMap<>();
		data.put("result", "OK");

		return ResponseEntity.status(HttpStatus.OK)
				.header("Access-Control-Allow-Headers", "accept, content-type")
				.header("Access-Control-Allow-Methods", "POST")
				.header("Access-Control-Allow-Origin", "*")
				.contentType(MediaType.APPLICATION_JSON_UTF8).body(data);
	}

	private HttpHeaders addResponseHeaders() {
		final HttpHeaders headers = new HttpHeaders();
		headers.add("Access-Control-Allow-Headers", "accept, content-type");
		headers.add("Access-Control-Allow-Methods", "POST");
		headers.add("Access-Control-Allow-Origin", "*");
		return headers;

	}

	private void addResponseHeaders(final HttpServletResponse response) {
		final HttpHeaders headers = addResponseHeaders();
		for (Entry<String, List<String>> entry : headers.entrySet()) {
			response.setHeader(entry.getKey(), entry.getValue().get(0));
		}
	}
}
