package example.service;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import example.controller.SearchController;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Service
@Component
@PropertySource("classpath:application.properties")

public class ExampleService {

	private static final Logger logger = LogManager.getLogger(ExampleService.class);

	@Value("${param}")
	private String param;

	public Map<String, Object> getDataMap(String data) {
		logger.info("ExampleService param= " + param);

		return new HashMap<String, Object>();
	}
}