package example.controller;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import example.utils.Utils;

@RestController
@RequestMapping("/")
public class Controller {

	private final Logger logger = LoggerFactory.getLogger(Controller.class);

	private static final Utils utils = Utils.getInstance();
	private static String result = null;

	@ResponseBody
	// 406 Not Acceptable client error response
	// 415 Unsupported Media Type
	@PostMapping(value = "write", /* consumes = MediaType.TEXT_PLAIN_VALUE, */ produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> page(@RequestParam String db,
			@RequestBody String payload) throws UnsupportedEncodingException {
		logger.info("body: " + payload);
		String input = URLDecoder.decode(payload.replaceFirst("db=" + db + "&", ""),
				"UTF-8");
		logger.info("input: " + input);
		result = utils.parseLineProtocolLine(input);
		logger.info("result: " + result);
		return ResponseEntity.status(HttpStatus.OK).body(result);
	}

	@ExceptionHandler({ UnsupportedEncodingException.class })
	public void handleException() {
		// TODO
	}
}
