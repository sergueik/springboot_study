package example.controller;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.Level;

import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import example.service.ExampleService;
import example.service.ExampleService;

@RestController
@RequestMapping("/")
public class DataSoureController {

	// @Autowired
	private ExampleService service;

	public DataSoureController(ExampleService data) {
		service = data;
	}

	private static final Logger logger = LogManager
			.getLogger(DataSoureController.class);

	@RequestMapping(value = "/", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public Map<String, Object> ReturnTest(HttpServletResponse response) {
		logger.info("g....../");
		response.setHeader("Access-Control-Allow-Headers", "accept, content-type");
		response.setHeader("Access-Control-Allow-Methods", "POST");
		response.setHeader("Access-Control-Allow-Origin", "*");

		Map<String, Object> map = new HashMap<>();
		map.put("result", "200 ok");
		return map;
	}

	@RequestMapping(value = "/search", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public List<String> Search(HttpServletResponse response) {
		logger.info("g....../search");
		response.setHeader("Access-Control-Allow-Headers", "accept, content-type");
		response.setHeader("Access-Control-Allow-Methods", "POST");
		response.setHeader("Access-Control-Allow-Origin", "*");

		List<String> result = new ArrayList<String>();
		result.add("DATA");
		return result;
	}

	@RequestMapping(value = "/query", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public List Query(@RequestBody Map<String, Object> params,
			HttpServletResponse response) {
		logger.info("g....../query");
		@SuppressWarnings("unchecked")
		List<Map<String, String>> targetList = (List<Map<String, String>>) params
				.get("targets");
		List<Map<String, Object>> result = new ArrayList<>();
		for (Map<String, String> targetMap : targetList) {
			String target = (String) targetMap.get("target");
			@SuppressWarnings("unchecked")
			Map<String, Object> scopedVars = (Map<String, Object>) params
					.get("scopedVars");
			@SuppressWarnings("unchecked")
			Map<String, String> IP = (Map<String, String>) scopedVars.get("IP");
			String nodeIP = (String) IP.get("text");
			switch (target) {

			case "DATA":
				result.add(service.getDataMap(nodeIP));
				break;
			default:
			}
		}
		response.setHeader("Access-Control-Allow-Headers", "accept, content-type");
		response.setHeader("Access-Control-Allow-Methods", "POST");
		response.setHeader("Access-Control-Allow-Origin", "*");
		Collections.sort(result, (o1, o2) -> {
			String name1 = String.valueOf(o1.get("target").toString());
			String name2 = String.valueOf(o2.get("target").toString());
			return name1.compareTo(name2);
		});
		return result;
	}

	@RequestMapping(value = "/annotations", method = RequestMethod.POST)
	@ResponseBody
	public Map<String, Object> Annotations() {
		logger.info("g....../annotations");
		Map<String, Object> map = new HashMap<>();
		map.put("result", "200 ok");
		return map;
	}

}
