package example.controller;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;

import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;
import org.springframework.context.annotation.Bean;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Controller;

import java.time.LocalDateTime;

// based on: https://github.com/kolorobot/spring-boot-thymeleaf
@Controller
@RequestMapping("/${application}")
public class HomeController {
	// NOTE: the value for annotation attribute
	// RequestParam.defaultValue must be a constant expression
	private final String defaultEnvKey = "APP_SERVER";
	private StringBuffer sb = new StringBuffer();

	// NOTE: application is a reserved variable name
	@Value("${application}")
	private String variable;

	private final String viewName = "index";
	private Log log = LogFactory.getLog(this.getClass());

	@GetMapping
	String index(Model model) {
		model.addAttribute("now", LocalDateTime.now());
		// https://stackoverflow.com/questions/56102116/access-application-properties-value-in-thymeleaf-template
		model.addAttribute("variable", variable);
		log.info("Setting text from property " + "application" + ":" + variable);
		model.addAttribute("hostname", showHostName());
		log.info(
				"Setting text from environment " + "hostname" + ":" + showHostName());
		return viewName;
	}

	public String showHostName() {
		sb = new StringBuffer();
		try {
			String hostname = InetAddress.getLocalHost().getHostName();
			sb.append("Server:" + hostname);
		} catch (UnknownHostException e) {
		}
		return sb.toString();
	}

	// https://www.baeldung.com/spring-request-param
	@ResponseBody
	@GetMapping("/env")
	public String showEnv(@RequestParam Optional<String> name) {
		log.info("showEnv name: " + (name.isPresent() ? name.get() : null));
		return _showEnv(name.isPresent() ? name.get() : null);
	}

	@GetMapping("/env2")
	@ResponseBody
	public String showEnv2(
			@RequestParam(defaultValue = defaultEnvKey) String name) {
		log.info("showEnv2 name: " + name);
		return _showEnv(name);
	}

	@ResponseBody
	@GetMapping("/env3/{name}")
	// NOTE: No mapping found for HTTP request with URI [/application/env3/]
	// unclear what is the point of required attribute property here
	public String showEnv3(@RequestParam(required = false) String name) {
		log.info("showEnv3 name: " + name);
		return _showEnv(name);
	}

	private String _showEnv(String name) {
		if (name != null) {
			// evaluate specific environment value
			return String.format("%s = %s", name, System.getenv(name));
		} else {
			// dump environment as String
			Map<String, String> systemEnvironment = System.getenv();
			sb.setLength(0);
			for (Entry<String, String> entry : systemEnvironment.entrySet()) {
				sb.append(entry.getKey() + " = " + entry.getValue() + "<br/>");
			}
			return sb.toString();
		}

	}

	// dump environment as JSON
	@ResponseBody
	@RequestMapping(method = {
			RequestMethod.GET }, value = "/env/json", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Map<String, String>> showEnvJSON() {
		Map<String, String> systemEnvironment = new HashMap<>();
		final List<String> keepKeys = Arrays.asList("USERNAME", "USER",
				"COMPUTERNAME", "WINDIR", "HOSTNAME");
		for (String key : System.getenv().keySet()) {
			String keyCheck = key;
			if (keepKeys.contains(keyCheck.toUpperCase())) {
				// continue;
				log.info("Adding the key to show: " + key);
				systemEnvironment.put(key, System.getenv(key));
			}
		}
		log.info("Returning object from environment "
				+ Arrays.asList(systemEnvironment.keySet()));
		return ResponseEntity.status(HttpStatus.OK).body(systemEnvironment);
	}
}
