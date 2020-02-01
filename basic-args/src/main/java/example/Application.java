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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Component
@RestController
@RequestMapping("/basic")
public class Application {

	@Autowired
	// not exposed about that params is linked to ApplicationArguments
	private Params params;

	private static final Logger logger = LoggerFactory
			.getLogger(CommandLineConfiguration.class);

	public void logConfiguration() {
		logger.info("Loaded with params: " + params.getId());
	}

	@GetMapping
	public String Hello() {
		final String appname = params.getAppname();
		final int result = params.getResult();
		return "This is " + appname + " and the result is: " + result;
	}
}
