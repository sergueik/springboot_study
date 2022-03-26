package example;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@SpringBootApplication
@RestController
@RequestMapping("/example")
public class Example {
	@Autowired
	LogHelper loghelper;

	public static void main(String[] args) {
		SpringApplication.run(Example.class, args);
		// can chain close
		// SpringApplication.run(Example.class, args).close();
	}

	// https://www.baeldung.com/spring-request-param
	// https://www.baeldung.com/java-url-encoding-decoding
	@GetMapping
	public String handler(@RequestParam(required = false) String data) {
		String decodedData = data;
		if (data != null) {
			loghelper.info("raw data: " + data);
			if (LogHelper.getLogger().isTraceEnabled()) {
				loghelper.trace("raw data: " + data);
			}
			try {
				decodedData = URLDecoder.decode(data,
						StandardCharsets.UTF_8.toString());
			} catch (UnsupportedEncodingException e) {
				// ignore
			}
			loghelper.info(String.format("handler received: %s", decodedData));
			return ("handler received: " + decodedData);
		} else {
			loghelper.info("handler received: no data");
			return ("handler received: no data");

		}
	}
}
