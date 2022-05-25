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
import java.util.List;

import org.json.CDL;

// TODO: mvn case
// @Controller
@RestController
@RequestMapping(path = "/")
public class WelcomeController {
	private final Logger logger = LoggerFactory
			.getLogger(WelcomeController.class);

	// NOTE: Rest Controller does not return the model even when
	// @ResponseBody is omitted
	@ResponseBody
	@GetMapping(path = "/welcome")
	public String welcome() {
		logger.info("called");
		return "welcome";
	}
}
